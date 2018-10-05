package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods.{DELETE, GET, POST, PUT}
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.http.CirceSyntax
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import io.circe.Json
import org.scalatest.{CancelAfterFailure, Inspectors}
import org.scalatest.concurrent.Eventually

import scala.collection.immutable

class ProjectsSpec extends BaseSpec with Eventually with Inspectors with CancelAfterFailure with CirceSyntax {

  private def validateProject(response: Json, payload: Json, base: String) = {

    response.getString("base") shouldEqual base
    response.getJson("prefixMappings") shouldEqual payload.getJson("prefixMappings")
  }

  "creating a project" should {

    val orgId  = genId()
    val projId = genId()
    val id     = s"$orgId/$projId"

    "fail if the permissions are missing" in {
      eventually {
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Unauthorized
          json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
        }
      }
    }

    "add projects/create, orgs/create, orgs/write permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> """projects/create","orgs/write","orgs/create""")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
      eventually {
        cl(Req(GET, s"$iamBase/acls/", headersUser)).mapJson { (json, result) =>
          json.getArray("acl").head.getArray("permissions").size shouldEqual 3
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "fail if the project name is missing" in {
      eventually {
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, Json.obj().toEntity)).mapJson { (_, result) =>
          result.status shouldEqual StatusCodes.BadRequest
        }
      }
    }

    "fail if the HTTP verb used is POST" in {
      cl(Req(POST, s"$adminBase/projects/$id", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.MethodNotAllowed
        json shouldEqual jsonContentOf("/admin/errors/method-not-supported.json", errorCtx)
      }
    }

    "fail if organization doesn't exist" in {

      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, projectReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.NotFound
        json shouldEqual jsonContentOf("/admin/errors/org-doesnt-exist.json", errorCtx)
      }
    }

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(orgId, 1L, "orgs")
      }

      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, projectReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(id, 1L)
      }
    }

    "fail if project already exists" in {
      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, projectReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/already-exists.json", errorCtx)
      }
    }

    "fail if organization is deprecated" in {
      cl(Req(DELETE, s"$adminBase/orgs/$orgId?rev=1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual createRespJson(orgId, 2L, "orgs")
      }

      cl(Req(PUT, s"$adminBase/projects/$orgId/${genId()}", headersUser, projectReqEntity())).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/resource-deprecated.json", errorCtx)
      }
    }

    "fail when the project segment is illegal" in {
      cl(Req(PUT, s"$adminBase/projects/$orgId/123=", headersUser, projectReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/admin/errors/illegal-param-project.json", errorCtx)
      }
    }
  }

  "fetching a project" should {

    val orgId  = genId()
    val projId = genId()
    val id     = s"$orgId/$projId"
    val name   = genString()
    val base   = s"${config.admin.uri.toString()}/${genString()}"
    val create = projectReqEntity(nxv = "nxv", person = "person", name = name, base = base)

    "fail if the permissions are missing" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(orgId, 1L, "orgs")
      }

      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(id, 1L)
      }

      cl(Req(uri = s"$adminBase/projects/$id", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Unauthorized
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }
    }

    "add projects/read permissions for user" in {
      val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "projects/read")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    "succeed if project exists" in {

      eventually {
        cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          validateProject(json, create.toJson.value, base)
          validateAdminResource(json, "Project", "projects", id, name, 1L, projId)
        }

        cl(Req(uri = s"$adminBase/projects/$id", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          validateProject(json, create.toJson.value, base)
          validateAdminResource(json, "Project", "projects", id, name, 1L, projId)
        }
      }
    }

    "return not found when fetching a non existing revision of a project" in {
      cl(Req(uri = s"$adminBase/projects/$id?rev=3", headers = headersUser)).mapResp { result =>
        result.status shouldEqual StatusCodes.NotFound
      }
    }

    "return not found when fetching a non existing project" in {
      eventually {
        cl(Req(uri = s"$adminBase/projects/$orgId/${genId()}", headers = headersUser)).mapResp { result =>
          result.status shouldEqual StatusCodes.NotFound
        }
      }
    }

  }

  "updating a project" should {

    val orgId  = genId()
    val projId = genId()
    val id     = s"$orgId/$projId"
    val name   = genString()
    val base   = s"${config.admin.uri.toString()}/${genString()}"
    val create = projectReqEntity(nxv = "nxv", person = "person", name = name, base = base)
    "fail if the permissions are missing" in {

      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(orgId, 1L, "orgs")
      }

      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(id, 1L)
      }

      val updateJson = projectReqEntity("/admin/projects/update.json", "nxv", "person")
      cl(Req(PUT, s"$adminBase/projects/$id?rev=1", headersUser, updateJson)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Unauthorized
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }

    }

    "add projects/write permissions for user" in {
      val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "projects/write")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    "fail when project does not exist" in {
      eventually {
        cl(Req(PUT, s"$adminBase/projects/$orgId/${genId()}?rev=1", headersUser, projectReqEntity())).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.NotFound
            json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
        }
      }
    }

    "succeed and fetch revisions" in {
      val nameRev2 = genString()
      val baseRev2 = s"${config.admin.uri.toString()}/${genString()}"
      val updateRev2 =
        projectReqEntity("/admin/projects/update.json", "nxv", "person", name = nameRev2, base = baseRev2)
      cl(Req(PUT, s"$adminBase/projects/$id?rev=1", headersUser, updateRev2)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual createRespJson(id, 2L)
      }

      val nameRev3 = genString()
      val baseRev3 = s"${config.admin.uri.toString()}/${genString()}"
      val updateRev3 =
        projectReqEntity("/admin/projects/update.json", "nxv", "person", name = nameRev3, base = baseRev3)
      updateRev2.dataBytes
      cl(Req(PUT, s"$adminBase/projects/$id?rev=2", headersUser, updateRev3)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual createRespJson(id, 3L)
      }

      cl(Req(uri = s"$adminBase/projects/$id", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, updateRev3.toJson.value, baseRev3)
        validateAdminResource(json, "Project", "projects", id, nameRev3, 3L, projId)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=3", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, updateRev3.toJson.value, baseRev3)
        validateAdminResource(json, "Project", "projects", id, nameRev3, 3L, projId)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=2", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, updateRev2.toJson.value, baseRev2)
        validateAdminResource(json, "Project", "projects", id, nameRev2, 2L, projId)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, create.toJson.value, base)
        validateAdminResource(json, "Project", "projects", id, name, 1L, projId)
      }
    }

    "fail when wrong revision is provided" in {

      cl(Req(PUT, s"$adminBase/projects/$id?rev=4", headersUser, projectReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
      }
    }

  }

  "deprecating a project" should {

    val orgId  = genId()
    val projId = genId()
    val id     = s"$orgId/$projId"
    val name   = genString()
    val base   = s"${config.admin.uri.toString()}/${genString()}"
    val create = projectReqEntity(nxv = "nxv", person = "person", name = name, base = base)

    "fail when wrong revision is provided" in {
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(orgId, 1L, "orgs")
      }

      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(id, 1L)
      }

      cl(Req(DELETE, s"$adminBase/projects/$id?rev=4", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
      }
    }

    "fail when project does not exist" in {
      cl(Req(DELETE, s"$adminBase/projects/$orgId/${genId()}?rev=1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.NotFound
        json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
      }
    }

    "fail when revision is not provided" in {
      cl(Req(DELETE, s"$adminBase/projects/$id", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/admin/errors/rev-not-provided.json", errorCtx)
      }
    }

    "succeed if project exists" in {
      cl(Req(DELETE, s"$adminBase/projects/$id?rev=1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual createRespJson(id, 2L)
      }

      cl(Req(uri = s"$adminBase/projects/$id", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, create.toJson.value, base)
        validateAdminResource(json, "Project", "projects", id, name, 2L, projId, true)
      }

      cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateProject(json, create.toJson.value, base)
        validateAdminResource(json, "Project", "projects", id, name, 1L, projId)
      }
    }

    "fail when deprecating twice" in {
      cl(Req(DELETE, s"$adminBase/projects/$id?rev=2", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/admin/errors/already-deprecated.json", errorCtx)
      }
    }

    "fail to update after deprecation" in {

      cl(Req(PUT, s"$adminBase/projects/$id?rev=2", headersUser, projectReqEntity(nxv = "nxv", person = "person")))
        .mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/already-deprecated.json", errorCtx)
        }
    }
  }

  "listing projects" should {

    "delete all ACLs for user" in cleanAcls

    "return unauthorized access if user has no permissions on / or particular project" in {
      eventually {
        cl(Req(GET, s"$adminBase/projects?size=1", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Unauthorized
          json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
        }
      }
    }

    "add projects/create permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> """projects/create","orgs/write","orgs/create""")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    val orgId = genId()

    val projectIds: immutable.Seq[String] = 1 to 5 map { _ =>
      s"$orgId/${genId()}"
    } sorted

    def projectListingResults(ids: Seq[String]): Json = {
      Json.arr(
        ids.map { id =>
          Json.obj(
            "_id" -> Json.fromString(s"$adminBase/projects/$id"),
            "_source" -> Json.obj(
              "@id" -> Json.fromString(s"$adminBase/projects/$id")
            )
          )
        }: _*
      )
    }

    def projectListingResultsWithScore(ids: Seq[String]): Json = {
      Json.arr(
        ids.map { id =>
          Json.obj(
            "_id"    -> Json.fromString(s"$adminBase/projects/$id"),
            "_score" -> Json.fromDoubleOrString(1.0),
            "_source" -> Json.obj(
              "@id" -> Json.fromString(s"$adminBase/projects/$id")
            )
          )
        }: _*
      )
    }

    "create project and grant project permissions to user" in {
      eventually {
        cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(orgId, 1L, "orgs")
        }
      }

      val permissionsJson =
        jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> """projects/read","projects/write""")).toEntity

      forAll(projectIds) { id =>
        val projId = id.split("/")(1)
        cl(
          Req(PUT,
              s"$adminBase/projects/$id",
              headersUser,
              projectReqEntity(nxv = s"nxv-$projId", person = s"person-$projId", name = projId)))
          .mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.Created
            json shouldEqual createRespJson(id, 1L)
          }

        cl(Req(PUT, s"$iamBase/acls/$id", headersGroup, permissionsJson)).mapResp { result =>
          result.status shouldEqual StatusCodes.OK
          result.entity.isKnownEmpty() shouldEqual true
        }

        //Get permission added
        eventually {
          cl(Req(uri = s"$adminBase/projects/$id/acls", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json should (equal(
              jsonContentOf("/iam/project-perms-response.json",
                            resourceIamCtx ++ replSub + (quote("{perms}") -> """projects/read", "projects/write""", quote(
                              "{path}")                                   -> id)))
              or equal(
                jsonContentOf(
                  "/iam/project-perms-response.json",
                  resourceIamCtx ++ replSub + (quote("{perms}") -> """projects/write", "projects/read""", quote(
                    "{path}")                                   -> id))))
          }
        }
      }
    }

    "return only projects the user has access to" in {
      val expectedResults = Json.obj(
        "@context" -> Json.fromString(config.prefixes.searchContext.toString),
        "_results" -> projectListingResults(projectIds),
        "_total"   -> Json.fromInt(projectIds.size),
        "_links" -> Json.obj(
          "_self" -> Json.fromString(s"$adminBase/projects")
        )
      )

      eventually {
        cl(Req(uri = s"$adminBase/projects", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual expectedResults
        }
      }
    }

    "return only projects the user has access to in reverse name order" in {
      val expectedResults = Json.obj(
        "@context" -> Json.fromString(config.prefixes.searchContext.toString),
        "_results" -> projectListingResults(projectIds.reverse),
        "_total"   -> Json.fromInt(projectIds.size),
        "_links" -> Json.obj(
          "_self" -> Json.fromString(s"$adminBase/projects?sort=-schema:name")
        )
      )

      eventually {
        cl(Req(uri = s"$adminBase/projects?sort=-schema:name", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual expectedResults
        }
      }
    }

    "return only the project that matches the query param" in {
      val id     = projectIds.lastOption.value
      val projId = id.split("/")(1)

      val expectedResults = Json.obj(
        "@context"  -> Json.fromString(config.prefixes.searchContext.toString),
        "_maxScore" -> Json.fromDoubleOrString(1.0),
        "_results"  -> projectListingResultsWithScore(Seq(id)),
        "_total"    -> Json.fromInt(1),
        "_links" -> Json.obj(
          "_self" -> Json.fromString(s"$adminBase/projects?q=$projId")
        )
      )

      eventually {
        cl(Req(uri = s"$adminBase/projects?q=$projId", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual expectedResults
        }
      }
    }

    "return search results with pagination" in {
      val expectedResults = Json.obj(
        "@context" -> Json.fromString(config.prefixes.searchContext.toString),
        "_results" -> projectListingResults(projectIds.slice(2, 4)),
        "_total"   -> Json.fromInt(projectIds.size),
        "_links" -> Json.obj(
          "_next"     -> Json.fromString(s"$adminBase/projects?from=4&size=2"),
          "_previous" -> Json.fromString(s"$adminBase/projects?from=0&size=2"),
          "_self"     -> Json.fromString(s"$adminBase/projects?from=2&size=2")
        )
      )

      eventually {
        cl(Req(uri = s"$adminBase/projects?from=2&size=2", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual expectedResults
        }
      }
    }

    "filter projects by deprecation status" in {

      val deprecatedProjects    = projectIds.take(2)
      val nonDeprecatedProjects = projectIds.drop(2)

      forAll(deprecatedProjects) { id =>
        cl(Req(DELETE, s"$adminBase/projects/$id?rev=1", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 2L)
        }
      }

      val expectedResults = Json.obj(
        "@context" -> Json.fromString(config.prefixes.searchContext.toString),
        "_results" -> projectListingResults(projectIds),
        "_total"   -> Json.fromInt(projectIds.size),
        "_links" -> Json.obj(
          "_self" -> Json.fromString(s"$adminBase/projects")
        )
      )

      val expectedResultsDeprecated = Json.obj(
        "@context" -> Json.fromString(config.prefixes.searchContext.toString),
        "_results" -> projectListingResults(deprecatedProjects),
        "_total"   -> Json.fromInt(deprecatedProjects.size),
        "_links" -> Json.obj(
          "_self" -> Json.fromString(s"$adminBase/projects?deprecated=true")
        )
      )

      val expectedResultsNonDeprecated = Json.obj(
        "@context" -> Json.fromString(config.prefixes.searchContext.toString),
        "_results" -> projectListingResults(nonDeprecatedProjects),
        "_total"   -> Json.fromInt(nonDeprecatedProjects.size),
        "_links" -> Json.obj(
          "_self" -> Json.fromString(s"$adminBase/projects?deprecated=false")
        )
      )

      eventually {
        cl(Req(uri = s"$adminBase/projects?deprecated=true", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual expectedResultsDeprecated
        }
      }

      eventually {
        cl(Req(uri = s"$adminBase/projects?deprecated=false", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual expectedResultsNonDeprecated
        }
      }

      eventually {
        cl(Req(uri = s"$adminBase/projects", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual expectedResults
        }
      }
    }
  }
}
