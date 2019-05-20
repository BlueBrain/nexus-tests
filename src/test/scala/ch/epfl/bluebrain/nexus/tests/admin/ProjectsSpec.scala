package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import io.circe.Json
import org.scalatest.{CancelAfterFailure, EitherValues, Inspectors}
import org.scalatest.concurrent.Eventually

import scala.collection.immutable

class ProjectsSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with EitherValues {

  private def validateProject(response: Json, payload: Json) = {
    response.getString("base") shouldEqual payload.getString("base")
    response.getString("vocab") shouldEqual payload.getString("vocab")
    response.getJson("apiMappings") shouldEqual payload.getJson("apiMappings")
  }

  "projects API" should {

    val orgId  = genId()
    val projId = genId()
    val id     = s"$orgId/$projId"

    "fail to create project if the permissions are missing" in {
      cl(Req(PUT, s"$adminBase/projects/$id", headersJsonUser, Json.obj().toEntity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Forbidden
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json")
      }
    }

    "add organizations/create permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersGroup)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev
        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "fail to create if the HTTP verb used is POST" in {
      cl(Req(POST, s"$adminBase/projects/$id", headersJsonUser, Json.obj().toEntity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.MethodNotAllowed
        json shouldEqual jsonContentOf("/admin/errors/method-not-supported.json")
      }
    }

    "create organization" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersJsonUser, orgReqEntity()))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    val description = s"$id project"
    val base        = s"${config.kg.uri.toString()}/resources/$id/_/"
    val vocab       = s"${config.kg.uri.toString()}/vocabs/$id/"
    val createJson =
      projectReqJson(nxv = "nxv", person = "person", description = description, base = base, vocab = vocab)
    val create = createJson.toEntity

    "return not found when fetching a non existing project" in {
      cl(Req(uri = s"$adminBase/projects/$orgId/${genId()}", headers = headersJsonUser))
        .mapResp(_.status shouldEqual StatusCodes.NotFound)
    }

    "clean permissions and add projects/create permissions" in {
      cleanAcls
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "projects/create")
      ).toEntity
      cl(Req(PATCH, s"$iamBase/acls/$orgId", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.Created)

    }

    "create project" in {
      cl(Req(PUT, s"$adminBase/projects/$id", headersJsonUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json.removeMetadata() shouldEqual createRespJson(id, 1L)
      }
    }

    "fail to create if project already exists" in {
      cl(Req(PUT, s"$adminBase/projects/$id", headersJsonUser, Json.obj().toEntity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/project-already-exists.json",
                                       Map(
                                         quote("{projLabel}") -> projId,
                                         quote("{orgId}")     -> orgId,
                                         quote("{projId}")    -> id
                                       ))
      }
    }

    "ensure that necessary permissions have been set in IAM" in {
      cl(Req(GET, s"$iamBase/acls/$id", headersJsonUser)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        acls._results.head.acl.head.permissions shouldEqual Set(
          "acls/read",
          "acls/write",
          "files/write",
          "organizations/create",
          "organizations/read",
          "organizations/write",
          "projects/create",
          "projects/read",
          "projects/write",
          "resolvers/write",
          "resources/read",
          "resources/write",
          "schemas/write",
          "views/write",
          "views/query",
          "storages/write"
        )
      }
    }

    "fetch the project" in {
      cl(Req(GET, s"$adminBase/projects/$id", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, createJson)
        validateAdminResource(json, "Project", "projects", id, description, 1L, projId)
      }
    }

    "fetch project by UUID" in {
      cl(Req(GET, s"$adminBase/orgs/$orgId", headersGroup)).mapJson { (orgJson, _) =>
        val orgUuid = orgJson.hcursor.get[String]("_uuid").right.value
        cl(Req(GET, s"$adminBase/projects/$id", headersUser)).mapJson { (projJson, _) =>
          val projectUuid = projJson.hcursor.get[String]("_uuid").right.value
          cl(Req(GET, s"$adminBase/projects/$orgUuid/$projectUuid", headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual projJson
          }
        }
      }
    }

    "return not found when fetching a non existing revision of a project" in {
      cl(Req(uri = s"$adminBase/projects/$id?rev=3", headers = headersJsonUser)).mapResp { result =>
        result.status shouldEqual StatusCodes.NotFound
      }
    }

    "update project and fetch revisions" in {
      val descRev2  = s"$description update 1"
      val baseRev2  = s"${config.admin.uri.toString()}/${genString()}/"
      val vocabRev2 = s"${config.admin.uri.toString()}/${genString()}/"
      val updateRev2Json =
        projectReqJson("/admin/projects/update.json",
                       "nxv",
                       "person",
                       description = descRev2,
                       base = baseRev2,
                       vocab = vocabRev2)
      val updateRev2 = updateRev2Json.toEntity
      cl(Req(PUT, s"$adminBase/projects/$id?rev=1", headersJsonUser, updateRev2)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeMetadata() shouldEqual createRespJson(id, 2L)
      }

      val descRev3  = s"$description update 2"
      val baseRev3  = s"${config.admin.uri.toString()}/${genString()}/"
      val vocabRev3 = s"${config.admin.uri.toString()}/${genString()}/"

      val updateRev3Json =
        projectReqJson("/admin/projects/update.json",
                       "nxv",
                       "person",
                       description = descRev3,
                       base = baseRev3,
                       vocab = vocabRev3)
      val updateRev3 = updateRev3Json.toEntity
      updateRev2.dataBytes
      cl(Req(PUT, s"$adminBase/projects/$id?rev=2", headersJsonUser, updateRev3)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeMetadata() shouldEqual createRespJson(id, 3L)
      }

      cl(Req(uri = s"$adminBase/projects/$id", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, updateRev3Json)
        validateAdminResource(json, "Project", "projects", id, descRev3, 3L, projId)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=3", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, updateRev3Json)
        validateAdminResource(json, "Project", "projects", id, descRev3, 3L, projId)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=2", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, updateRev2Json)
        validateAdminResource(json, "Project", "projects", id, descRev2, 2L, projId)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, createJson)
        validateAdminResource(json, "Project", "projects", id, description, 1L, projId)
      }
    }

    "reject update  when wrong revision is provided" in {

      cl(Req(PUT, s"$adminBase/projects/$id?rev=4", headersJsonUser, Json.obj().toEntity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/project-incorrect-revision.json")
      }
    }

    "deprecate project" in {
      cl(Req(DELETE, s"$adminBase/projects/$id?rev=3", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeMetadata() shouldEqual createRespJson(id, 4L, deprecated = true)
      }

      cl(Req(uri = s"$adminBase/projects/$id", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Project", "projects", id, s"$description update 2", 4L, projId, true)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, createJson)
        validateAdminResource(json, "Project", "projects", id, description, 1L, projId)
      }
    }

    "prevent fetching a project if permissions are missing" in {
      cleanAcls
      cl(Req(uri = s"$adminBase/projects/$id", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Forbidden
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }
    }

  }

  "listing projects" should {

    "delete all ACLs for user" in cleanAcls

    "return unauthorized access if user has no permissions on / " in {
      cl(Req(GET, s"$adminBase/projects", headersJsonUser, Json.obj().toEntity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual jsonContentOf("/admin/projects/empty-project-list.json")
      }
    }

    "add projects/create permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create\",\"projects/read")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersGroup)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersGroup, json)).mapResp(_.status shouldEqual StatusCodes.OK)
      }

    }

    val orgId = genId()

    val projectIds: immutable.Seq[String] = 1 to 5 map { _ =>
      s"$orgId/${genId()}"
    } sorted

    def projectListingResults(ids: Seq[String]): Json = {
      Json.arr(
        ids.map { id =>
          jsonContentOf(
            "/admin/projects/listing-item.json",
            Map(
              quote("{adminBase}") -> adminBase.toString(),
              quote("{id}")        -> id,
              quote("{projId}")    -> id.split("/")(1),
              quote("{orgId}")     -> id.split("/")(0),
              quote("{iamBase}")   -> config.iam.uri.toString(),
              quote("{user}")      -> config.iam.userSub
            )
          )
        }: _*
      )
    }

    "create projects" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersJsonUser, orgReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json.removeMetadata() shouldEqual createRespJson(orgId, 1L, "orgs", "Organization")
      }

      forAll(projectIds) { id =>
        val projId = id.split("/")(1)
        cl(
          Req(
            PUT,
            s"$adminBase/projects/$id",
            headersJsonUser,
            projectReqJson(nxv = s"nxv-$projId",
                           person = s"person-$projId",
                           description = projId,
                           base = s"http:example.com/$projId/",
                           vocab = s"http:example.com/$projId/vocab/").toEntity
          ))
          .mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.Created
            json.removeMetadata() shouldEqual createRespJson(id, 1L)
          }

      }
    }

    "list projects" in {
      val expectedResults = Json.obj(
        "@context" -> Json.arr(
          Json.fromString("https://bluebrain.github.io/nexus/contexts/admin.json"),
          Json.fromString("https://bluebrain.github.io/nexus/contexts/resource.json"),
          Json.fromString("https://bluebrain.github.io/nexus/contexts/search.json")
        ),
        "_total"   -> Json.fromInt(projectIds.size),
        "_results" -> projectListingResults(projectIds),
      )

      cl(Req(uri = s"$adminBase/projects/$orgId", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        removeSearchMetadata(json) shouldEqual expectedResults
      }
    }

    "list projects which user has access to" in {
      cleanAcls
      val projectsToList = projectIds.slice(0, 2)
      val expectedResults = Json.obj(
        "@context" -> Json.arr(
          Json.fromString("https://bluebrain.github.io/nexus/contexts/admin.json"),
          Json.fromString("https://bluebrain.github.io/nexus/contexts/resource.json"),
          Json.fromString("https://bluebrain.github.io/nexus/contexts/search.json")
        ),
        "_total"   -> Json.fromInt(projectsToList.size),
        "_results" -> projectListingResults(projectsToList),
      )
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "projects/read")
      ).toEntity

      projectsToList.foreach { projectId =>
        cl(Req(PATCH, s"$iamBase/acls/$projectId", headersGroup, json))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

      cl(Req(uri = s"$adminBase/projects/$orgId", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        removeSearchMetadata(json) shouldEqual expectedResults
      }
    }
  }

  def removeSearchMetadata(json: Json): Json =
    json.hcursor
      .downField("_results")
      .withFocus(
        _.mapArray(
          _.map(
            _.removeFields("_createdAt", "_updatedAt", "_uuid", "_organizationUuid")
          )
        )
      )
      .top
      .value
}
