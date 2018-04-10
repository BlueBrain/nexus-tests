package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{RequestEntity, StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.http.JsonOps._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, OptionValues}

class AdminSpec extends BaseSpec with OptionValues with CancelAfterFailure with Eventually {

  private val startPool = Vector.range('a', 'z')
  private val pool      = Vector.range('a', 'z') ++ Vector.range('0', '9') :+ '_' :+ '-'
  private val adminBase = config.admin.uri
  private val iamBase   = config.iam.uri

  private def randomProjectPrefix = genString(1, startPool) + genString(genInt(10), pool)

  private def genId(length: Int = 15): String =
    genString(length = length, Vector.range('a', 'z') ++ Vector.range('0', '9'))

  private def reqEntity(path: String = "/admin/create.json",
                        nxv: String = randomProjectPrefix,
                        person: String = randomProjectPrefix): RequestEntity = {
    val rep = Map(quote("{nxv-prefix}") -> nxv,
                  quote("{person-prefix}") -> person,
                  quote("{name}")          -> genString(),
                  quote("{desc}")          -> genString())
    jsonContentOf(path, rep).toEntity
  }

  private def createRespJson(id: String, rev: Long): Json = {
    val resp = resourceCtx ++ Map(quote("{base}") -> config.admin.uri.toString(),
                                  quote("{id}")        -> id,
                                  quote(""""{rev}"""") -> rev.toString)
    jsonContentOf("/admin/response.json", resp)
  }

  private def entityMeta(id: String, rev: Long, deprecated: Boolean = false): Json =
    Json.obj(
      "@context"       -> Json.fromString(config.prefixes.coreContext.toString()),
      "@id"            -> Json.fromString(s"$adminBase/projects/$id"),
      "@type"          -> Json.fromString("nxv:Project"),
      "nxv:rev"        -> Json.fromLong(rev),
      "nxv:deprecated" -> Json.fromBoolean(deprecated)
    )

  "An Admin service" when {
    val replSub = Map(quote("{sub}") -> config.iam.userSub)

    "managing ACLs" should {

      "delete all ACLs for user" in {

        cl(Req(uri = s"$iamBase/acls/", headers = headersGroup)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          val permissions = json.hcursor.get[Vector[Json]]("acl").toOption.value.foldLeft(Set.empty[String]) {
            case (acc, j) =>
              j.hcursor.downField("identity").get[String]("sub") match {
                case Right(s) if s == config.iam.userSub =>
                  j.hcursor.get[Set[String]]("permissions").toOption.value ++ acc
                case _ => acc
              }
          }

          if (permissions.nonEmpty) {
            val entity =
              jsonContentOf("/iam/patch-single.json", replSub + (quote("{perms}") -> permissions.mkString("""",""""))).toEntity

            cl(Req(PATCH, s"$iamBase/acls/", headersGroup, entity)).mapJson { (json, result) =>
              result.status shouldEqual StatusCodes.OK
              json shouldEqual jsonContentOf("/iam/patch-response.json", replSub ++ resourceIamCtx)
            }
          } else permissions shouldEqual Set.empty[String]
        }
      }
    }

    "creating a project" should {

      "fail if the permissions are missing" in {
        eventually {
          cl(Req(PUT, s"$adminBase/projects/${genId()}", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.Unauthorized
            json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
          }
        }
      }

      "add projects/create permissions for user" in {
        val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "projects/create")).toEntity
        cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
          result.status shouldEqual StatusCodes.OK
          result.entity.isKnownEmpty() shouldEqual true
        }
      }

      "fail if the project name is missing" in {
        eventually {
          cl(Req(PUT, s"$adminBase/projects/${genId()}", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.BadRequest
            json.removeKeys("violations") shouldEqual jsonContentOf("/admin/errors/create-no-name-resp.json", errorCtx)
          }
        }
      }

      "fail if the HTTP verb used is POST" in {
        cl(Req(POST, s"$adminBase/projects/${genId()}", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.MethodNotAllowed
          json shouldEqual jsonContentOf("/admin/errors/method-not-supported.json", errorCtx)
        }
      }

      "succeed if payload is correct" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }
      }

      "fail if project already exists" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/already-exists.json", errorCtx)
        }
      }

      "fail when the project segment is illegal" in {
        cl(Req(PUT, s"$adminBase/projects/123K=", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/illegal-param.json", errorCtx)
        }
      }
    }

    "fetching a project" should {

      "fail if the permissions are missing" in {
        val id     = genId()
        val create = reqEntity(nxv = "nxv", person = "person")

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

      "return not found when fetching a non existing project" in {
        eventually {
          cl(Req(uri = s"$adminBase/projects/${genId()}", headers = headersUser)).mapResp { result =>
            result.status shouldEqual StatusCodes.NotFound
          }
        }
      }

      "return not found when fetching a non existing revision of a project" in {
        val id = genId()
        eventually {
          cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.Created
            json shouldEqual createRespJson(id, 1L)
          }
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=3", headers = headersUser)).mapResp { result =>
          result.status shouldEqual StatusCodes.NotFound
        }
      }

      "succeed if project exists" in {
        val id     = genId()
        val create = reqEntity(nxv = "nxv", person = "person")

        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, create)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }

        cl(Req(uri = s"$adminBase/projects/$id", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }
      }
    }

    "updating a project" should {

      "fail if the permissions are missing" in {

        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity(nxv = "nxv", person = "person"))).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.Created
            json shouldEqual createRespJson(id, 1L)
        }

        val updateJson = reqEntity("/admin/update.json", "nxv", "person")
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

      "fail if the payload overrides prefixMappings object" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        eventually {
          cl(Req(PUT, s"$adminBase/projects/$id?rev=1", headersUser, reqEntity())).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.BadRequest
            json shouldEqual jsonContentOf("/admin/errors/update-overriden-prefixes.json", errorCtx)
          }
        }
      }

      "fail when wrong revision is provided" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id?rev=4", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
        }
      }

      "fail when project does not exist" in {
        cl(Req(PUT, s"$adminBase/projects/${genId()}?rev=1", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.NotFound
          json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
        }
      }

      "succeed and fetch revisions" in {
        val id     = genId()
        val create = reqEntity(nxv = "nxv", person = "person")
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, create)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        val updateRev1 = reqEntity("/admin/update.json", "nxv", "person")
        cl(Req(PUT, s"$adminBase/projects/$id?rev=1", headersUser, updateRev1)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 2L)
        }

        val updateRev2 = reqEntity("/admin/update.json", "nxv", "person")
        updateRev1.dataBytes
        cl(Req(PUT, s"$adminBase/projects/$id?rev=2", headersUser, updateRev2)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 3L)
        }

        cl(Req(uri = s"$adminBase/projects/$id", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (updateRev2.toJson.value deepMerge entityMeta(id, 3L))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=3", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (updateRev2.toJson.value deepMerge entityMeta(id, 3L))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=2", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (updateRev1.toJson.value deepMerge entityMeta(id, 2L))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }
      }
    }

    "deprecating a project" should {

      "fail when wrong revision is provided" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=4", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
        }
      }

      "fail when project does not exist" in {
        cl(Req(DELETE, s"$adminBase/projects/${genId()}?rev=1", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.NotFound
          json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
        }
      }

      "fail when revision is not provided" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/rev-not-provided.json", errorCtx)
        }
      }

      "succeed if project exists" in {
        val id     = genId()
        val create = reqEntity()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, create)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=1", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 2L)
        }

        cl(Req(uri = s"$adminBase/projects/$id", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 2L, deprecated = true))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }
      }

      "fail when deprecating twice" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=1", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 2L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=2", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/already-deprecated.json", errorCtx)
        }
      }

      "fail to update after deprecation" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity(nxv = "nxv", person = "person"))).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.Created
            json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id?rev=1", headersUser, reqEntity(nxv = "nxv", person = "person"))).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual createRespJson(id, 2L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=2", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 3L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id?rev=3", headersUser, reqEntity(nxv = "nxv", person = "person"))).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.BadRequest
            json shouldEqual jsonContentOf("/admin/errors/already-deprecated.json", errorCtx)
        }
      }
    }

    "checking ACLs" should {

      "delete projects/read permissions for user" in {
        val entity = jsonContentOf("/iam/patch-single.json", replSub + (quote("{perms}") -> "projects/read")).toEntity
        cl(Req(PATCH, s"$iamBase/acls/", headersGroup, entity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf(
            "/iam/patch-response-perms.json",
            replSub ++ resourceIamCtx + (quote("{perms}") -> """projects/create","projects/write"""))
        }
      }

      "fail if the projects/read permissions are missing" in {
        eventually {
          cl(Req(uri = s"$adminBase/projects/${genId()}/acls", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.NotFound
            json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
          }
        }
      }

      "add projects/read permissions for user" in {
        val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "projects/read")).toEntity
        cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
          result.status shouldEqual StatusCodes.OK
          result.entity.isKnownEmpty() shouldEqual true
        }
      }

      "return not found when project does not exist" in {
        eventually {
          cl(Req(uri = s"$adminBase/projects/${genId()}/acls", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.NotFound
            json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
          }
        }
      }

      "return underlying ACLs for a project with parents=true query param" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }
        eventually {
          cl(Req(uri = s"$adminBase/projects/$id/acls?parents=true", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual jsonContentOf(
              "/iam/project-perms-response.json",
              resourceIamCtx ++ replSub + (quote("{perms}") -> """projects/create","projects/write","projects/read""", quote(
                "{path}")                                   -> "")
            )
          }
        }
      }

      "return empty ACLs for path without them" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(uri = s"$adminBase/projects/$id/acls", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/iam/project-perms-response-empty.json", resourceIamCtx)
        }
      }

      "add own permissions for user" in {
        val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "own")).toEntity
        cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
          result.status shouldEqual StatusCodes.OK
          result.entity.isKnownEmpty() shouldEqual true
        }
      }

      "add/subtract/delete/fetch permissions from a specific project" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        //Add random permissions to a project
        val permission = genString()
        val aclToAdd   = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> permission))
        eventually {
          cl(Req(PUT, s"$adminBase/projects/$id/acls", headersUser, aclToAdd.toEntity)).mapResp { result =>
            result.status shouldEqual StatusCodes.OK
            result.entity.isKnownEmpty() shouldEqual true
          }
        }

        //Get permission added
        eventually {
          cl(Req(uri = s"$adminBase/projects/$id/acls", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual jsonContentOf(
              "/iam/project-perms-response.json",
              resourceIamCtx ++ replSub + (quote("{perms}") -> permission, quote("{path}") -> id))
          }
        }

        //Add another random permission to a project
        val permission2 = genString()
        val aclToAdd2   = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> permission2))
        cl(Req(PUT, s"$adminBase/projects/$id/acls", headersUser, aclToAdd2.toEntity)).mapResp { result =>
          result.status shouldEqual StatusCodes.OK
          result.entity.isKnownEmpty() shouldEqual true
        }

        //Get permission added
        eventually {
          cl(Req(uri = s"$adminBase/projects/$id/acls", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual jsonContentOf(
              "/iam/project-perms-response.json",
              resourceIamCtx ++ replSub + (quote("{perms}") -> s"""$permission","$permission2""", quote("{path}") -> id))
          }
        }

        //Subtract the first added permission
        val entity = jsonContentOf("/iam/patch-single.json", replSub + (quote("{perms}") -> permission)).toEntity
        cl(Req(PATCH, s"$adminBase/projects/$id/acls", headersUser, entity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual jsonContentOf("/iam/patch-response-perms.json",
                                         replSub ++ resourceIamCtx + (quote("{perms}") -> permission2))
        }

        //Get permission after subtraction
        eventually {
          cl(Req(uri = s"$adminBase/projects/$id/acls", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual jsonContentOf(
              "/iam/project-perms-response.json",
              resourceIamCtx ++ replSub + (quote("{perms}") -> permission2, quote("{path}") -> id))
          }
        }

        //Delete permissions for the project path
        cl(Req(DELETE, s"$adminBase/projects/$id/acls", headersUser)).mapResp { result =>
          result.status shouldEqual StatusCodes.NoContent
          result.entity.isKnownEmpty() shouldEqual true
        }

        //Get empty permissions
        eventually {
          cl(Req(uri = s"$adminBase/projects/$id/acls", headers = headersUser)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual jsonContentOf("/iam/project-perms-response-empty.json", resourceIamCtx)
          }
        }
      }
    }
  }
}
