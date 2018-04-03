package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{RequestEntity, StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.http.JsonOps._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import io.circe.Json
import org.scalatest.OptionValues

class AdminSpec extends BaseSpec with OptionValues {

  private val startPool = Vector.range('a', 'z')
  private val pool      = Vector.range('a', 'z') ++ Vector.range('0', '9') :+ '_' :+ '-'
  private val adminBase = config.admin.uri

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

    "creating a project" should {

      "fail if the project name is missing" in {
        cl(Req(PUT, s"$adminBase/projects/${genId()}", entity = Json.obj().toEntity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json.removeKeys("violations") shouldEqual jsonContentOf("/admin/errors/create-no-name-resp.json", errorCtx)
        }
      }

      "fail if the HTTP verb used is POST" in {
        cl(Req(POST, s"$adminBase/projects/${genId()}", entity = Json.obj().toEntity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.MethodNotAllowed
          json shouldEqual jsonContentOf("/admin/errors/method-not-supported.json", errorCtx)
        }
      }

      "succeed if payload is correct" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }
      }

      "fail if project already exists" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/already-exists.json", errorCtx)
        }
      }

      "fail when the project segment is illegal" in {
        cl(Req(PUT, s"$adminBase/projects/123K=", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/illegal-param.json", errorCtx)
        }
      }
    }

    "fetching a project" should {
      "return not found when fetching a non existing project" in {
        cl(Req(uri = s"$adminBase/projects/${genId()}")).mapResp { result =>
          result.status shouldEqual StatusCodes.NotFound
        }
      }

      "return not found when fetching a non existing revision of a project" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=3")).mapResp { result =>
          result.status shouldEqual StatusCodes.NotFound
        }
      }

      "succeed if project exists" in {
        val id     = genId()
        val create = reqEntity(nxv = "nxv", person = "person")

        cl(Req(PUT, s"$adminBase/projects/$id", entity = create)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=1")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }

        cl(Req(uri = s"$adminBase/projects/$id")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }
      }
    }

    "updating a project" should {

      "fail if the payload overrides prefixMappings object" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id?rev=1", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/update-overriden-prefixes.json", errorCtx)
        }
      }

      "fail when wrong revision is provided" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id?rev=4", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
        }
      }

      "fail when project does not exist" in {
        cl(Req(PUT, s"$adminBase/projects/${genId()}?rev=1", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.NotFound
          json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
        }
      }

      "succeed and fetch revisions" in {
        val id     = genId()
        val create = reqEntity(nxv = "nxv", person = "person")
        cl(Req(PUT, s"$adminBase/projects/$id", entity = create)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        val updateRev1 = reqEntity("/admin/update.json", "nxv", "person")
        cl(Req(PUT, s"$adminBase/projects/$id?rev=1", entity = updateRev1)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 2L)
        }

        val updateRev2 = reqEntity("/admin/update.json", "nxv", "person")
        updateRev1.dataBytes
        cl(Req(PUT, s"$adminBase/projects/$id?rev=2", entity = updateRev2)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 3L)
        }

        cl(Req(uri = s"$adminBase/projects/$id")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (updateRev2.toJson.value deepMerge entityMeta(id, 3L))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=3")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (updateRev2.toJson.value deepMerge entityMeta(id, 3L))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=2")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (updateRev1.toJson.value deepMerge entityMeta(id, 2L))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=1")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }
      }
    }

    "deprecating a project" should {

      "fail when wrong revision is provided" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=4")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
        }
      }

      "fail when project does not exist" in {
        cl(Req(DELETE, s"$adminBase/projects/${genId()}?rev=1")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.NotFound
          json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
        }
      }

      "fail when revision is not provided" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/rev-not-provided.json", errorCtx)
        }
      }

      "succeed if project exists" in {
        val id     = genId()
        val create = reqEntity()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = create)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=1")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 2L)
        }

        cl(Req(uri = s"$adminBase/projects/$id")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 2L, deprecated = true))
        }

        cl(Req(uri = s"$adminBase/projects/$id?rev=1")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual (create.toJson.value deepMerge entityMeta(id, 1L))
        }
      }

      "fail when deprecating twice" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=1")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 2L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=2")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/already-deprecated.json", errorCtx)
        }
      }

      "fail to update after deprecation" in {
        val id = genId()
        cl(Req(PUT, s"$adminBase/projects/$id", entity = reqEntity(nxv = "nxv", person = "person"))).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.Created
            json shouldEqual createRespJson(id, 1L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id?rev=1", entity = reqEntity(nxv = "nxv", person = "person"))).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual createRespJson(id, 2L)
        }

        cl(Req(DELETE, s"$adminBase/projects/$id?rev=2")).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual createRespJson(id, 3L)
        }

        cl(Req(PUT, s"$adminBase/projects/$id?rev=3", entity = reqEntity(nxv = "nxv", person = "person"))).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.BadRequest
            json shouldEqual jsonContentOf("/admin/errors/already-deprecated.json", errorCtx)
        }
      }
    }
  }
}
