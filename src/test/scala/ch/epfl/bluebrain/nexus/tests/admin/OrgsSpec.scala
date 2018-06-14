package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.http.JsonOps._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, OptionValues}

class OrgsSpec extends BaseSpec with OptionValues with CancelAfterFailure with Eventually {

  "managing ACLs" should {

    "delete all ACLs for user" in cleanAcls
  }

  "creating an organization" should {

    "fail if the permissions are missing" in {
      eventually {
        cl(Req(PUT, s"$adminBase/orgs/${genId()}", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Unauthorized
          json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
        }
      }
    }

    "add organizations/create permissions for user" in {
      val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "orgs/create")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    "fail if the organizations name is missing" in {
      eventually {
        cl(Req(PUT, s"$adminBase/orgs/${genId()}", headersUser, Json.obj().toEntity)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json.removeKeys("violations") shouldEqual jsonContentOf("/admin/errors/create-no-name-resp.json", errorCtx)
        }
      }
    }

    "succeed if payload is correct" in {
      val id = genId()
      cl(Req(PUT, s"$adminBase/orgs/$id", headersUser, orgReqEntity("/admin/orgs/payload.json"))).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L, "orgs")
      }
    }

    "fail if organization already exists" in {
      val id = genId()
      cl(Req(PUT, s"$adminBase/orgs/$id", headersUser, orgReqEntity("/admin/orgs/payload.json"))).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L, "orgs")
      }

      cl(Req(PUT, s"$adminBase/orgs/$id", headersUser, orgReqEntity("/admin/orgs/payload.json"))).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/already-exists.json", errorCtx)
      }

    }

    "fail when the organization segment is illegal" in {
      cl(Req(PUT, s"$adminBase/orgs/123K=", headersUser, orgReqEntity("/admin/orgs/payload.json"))).mapJson {
        (json, result) =>
          result.status shouldEqual StatusCodes.BadRequest
          json shouldEqual jsonContentOf("/admin/errors/illegal-param-org.json", errorCtx)
      }
    }
  }

  "fetching an organization" should {
    val id     = genId()
    val name   = genString()
    val create = orgReqEntity(name)
    "fail if the permissions are missing" in {

      cl(Req(PUT, s"$adminBase/orgs/$id", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(id, 1L, "orgs")
      }

      cl(Req(uri = s"$adminBase/orgs/$id", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Unauthorized
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }
    }

    "add orgs/read permissions for user" in {
      val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "orgs/read")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    "succeed if organization exists" in {
      eventually {
        cl(Req(uri = s"$adminBase/orgs/$id", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          validateAdminResource(json, "Organization", "orgs", id, name, 1L, id)

        }
      }
    }

    "return not found when fetching a non existing revision of an organizations" in {
      cl(Req(uri = s"$adminBase/orgs/$id?rev=3", headers = headersUser)).mapResp { result =>
        result.status shouldEqual StatusCodes.NotFound
      }
    }

    "return not found when fetching a non existing organization" in {
      eventually {
        cl(Req(uri = s"$adminBase/orgs/${genId()}", headers = headersUser)).mapResp { result =>
          result.status shouldEqual StatusCodes.NotFound
        }
      }
    }
  }

  "updating an organization" should {

    val id     = genId()
    val name   = genString()
    val create = orgReqEntity(name)

    "fail if the permissions are missing" in {

      cl(Req(PUT, s"$adminBase/orgs/$id", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(id, 1L, "orgs")
      }

      cl(Req(PUT, s"$adminBase/orgs/$id?rev=1", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Unauthorized
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }

    }

    "add orgs/write permissions for user" in {
      val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "orgs/write")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    "fail when wrong revision is provided" in {
      eventually {
        cl(Req(PUT, s"$adminBase/orgs/$id?rev=4", headersUser, create)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Conflict
          json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
        }
      }
    }

    "fail when project does not exist" in {
      cl(Req(PUT, s"$adminBase/orgs/${genId()}?rev=1", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.NotFound
        json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
      }
    }

    "succeed and fetch revisions" in {

      val updatedName = genString()
      val update      = orgReqEntity(updatedName)

      cl(Req(PUT, s"$adminBase/orgs/$id?rev=1", headersUser, update)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual createRespJson(id, 2L, "orgs")
      }

      val updatedName2 = genString()
      val update2      = orgReqEntity(updatedName2)

      cl(Req(PUT, s"$adminBase/orgs/$id?rev=2", headersUser, update2)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual createRespJson(id, 3L, "orgs")

      }

      cl(Req(uri = s"$adminBase/orgs/$id", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, updatedName2, 3, id)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=3", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, updatedName2, 3L, id)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=2", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, updatedName, 2L, id)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, name, 1L, id)
      }
    }

  }

  "deprecating an organization" should {
    val id     = genId()
    val name   = genString()
    val create = orgReqEntity(name)

    "fail when wrong revision is provided" in {
      cl(Req(PUT, s"$adminBase/orgs/$id", headersUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(id, 1L, "orgs")
      }

      cl(Req(DELETE, s"$adminBase/orgs/$id?rev=4", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/incorrect-revision.json", errorCtx)
      }
    }

    "fail when project does not exist" in {
      cl(Req(DELETE, s"$adminBase/orgs/${genId()}?rev=1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.NotFound
        json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
      }
    }

    "fail when revision is not provided" in {
      cl(Req(DELETE, s"$adminBase/orgs/$id", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/admin/errors/rev-not-provided.json", errorCtx)
      }
    }

    "succeed if organization exists" in {
      cl(Req(DELETE, s"$adminBase/orgs/$id?rev=1", headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json shouldEqual createRespJson(id, 2L, "orgs")
      }

      cl(Req(uri = s"$adminBase/orgs/$id", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, name, 2L, id,true)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=1", headers = headersUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, name, 1L, id)

      }
    }
  }
}
