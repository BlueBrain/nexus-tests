package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import io.circe.Json
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, EitherValues, OptionValues}

class OrgsSpec extends BaseSpec with OptionValues with CancelAfterFailure with Eventually with EitherValues {

  "creating an organization" should {

    "fail if the permissions are missing" in {
      cl(Req(PUT, s"$adminBase/orgs/${genId()}", headersJsonUser, Json.obj().toEntity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Forbidden
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json")
      }
    }

    "add necessary permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersServiceAccount, json))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    val id = genId()

    "succeed if payload is correct" in {
      cl(Req(PUT, s"$adminBase/orgs/$id", headersJsonUser, orgReqEntity(id))).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json.removeMetadata() shouldEqual createRespJson(id, 1L, "orgs", "Organization")
      }
    }

    "check if permissions have been created for user" in {
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

    "fail if organization already exists" in {
      val id = genId()
      cl(Req(PUT, s"$adminBase/orgs/$id", headersJsonUser, orgReqEntity(id))).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json.removeMetadata() shouldEqual createRespJson(id, 1L, "orgs", "Organization")
      }

      cl(Req(PUT, s"$adminBase/orgs/$id", headersJsonUser, orgReqEntity(id))).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/org-already-exists.json", Map(quote("{orgId}") -> id))
      }

    }

  }

  "fetching an organization" should {
    val id     = genId()
    val create = orgReqEntity(s"$id organization")
    "fail if the permissions are missing" in {

      cl(Req(PUT, s"$adminBase/orgs/$id", headersJsonUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json.removeMetadata() shouldEqual createRespJson(id, 1L, "orgs", "Organization")
      }

      cleanAcls
      cl(Req(uri = s"$adminBase/orgs/$id", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Forbidden
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }
    }

    "add orgs/read permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/read")
      ).toEntity

      cl(Req(PATCH, s"$iamBase/acls/$id?rev=0", headersServiceAccount, json))
        .mapResp(_.status shouldEqual StatusCodes.Created)

    }

    "succeed if organization exists" in {
      cl(Req(uri = s"$adminBase/orgs/$id", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, s"$id organization", 1L, id)
      }
    }

    "fetch organization by UUID" in {
      cl(Req(GET, s"$adminBase/orgs/$id", headersUser)).mapJson { (orgJson, _) =>
        val orgUuid = orgJson.hcursor.get[String]("_uuid").right.value
        cl(Req(GET, s"$adminBase/orgs/$orgUuid/", headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json shouldEqual orgJson
        }
      }
    }

    "return not found when fetching a non existing revision of an organizations" in {
      cl(Req(uri = s"$adminBase/orgs/$id?rev=3", headers = headersJsonUser)).mapResp { result =>
        result.status shouldEqual StatusCodes.NotFound
      }
    }

    val nonExistent = genId()
    "add orgs/read permissions for non-existing organization" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/read")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/$nonExistent", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.headOption.map(_._rev).getOrElse(0)

        cl(Req(PATCH, s"$iamBase/acls/$nonExistent?rev=$rev", headersServiceAccount, json))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

    }

    "return not found when fetching a non existing organization" in {
      cl(Req(uri = s"$adminBase/orgs/$nonExistent", headers = headersJsonUser)).mapResp { result =>
        result.status shouldEqual StatusCodes.NotFound
      }
    }
  }

  "updating an organization" should {

    val id     = genId()
    val create = orgReqEntity(s"$id organization")

    "add orgs/create permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create")
      ).toEntity

      cl(Req(PATCH, s"$iamBase/acls/$id?rev=0", headersServiceAccount, json))
        .mapResp(_.status shouldEqual StatusCodes.Created)

    }

    "create organization" in {
      cl(Req(PUT, s"$adminBase/orgs/$id", headersJsonUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json.removeMetadata() shouldEqual createRespJson(id, 1L, "orgs", "Organization")
      }
    }

    "fail if the permissions are missing" in {
      cleanAcls
      cl(Req(PUT, s"$adminBase/orgs/$id?rev=1", headersJsonUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Forbidden
        json shouldEqual jsonContentOf("/iam/errors/unauthorized-access.json", errorCtx)
      }

    }

    "add orgs/write permissions for user" in {
      val json = jsonContentOf("/iam/add.json",
                               replSub + (quote("{perms}") -> "organizations/write\",\"organizations/read")).toEntity
      cl(Req(PUT, s"$iamBase/acls/$id", headersServiceAccount, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.Created
      }
    }

    "fail when wrong revision is provided" in {
      cl(Req(PUT, s"$adminBase/orgs/$id?rev=4", headersJsonUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/org-incorrect-revision.json", errorCtx)
      }
    }

    val nonExistent = genId()
    "add orgs/read permissions for non-existing organization" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/write")
      ).toEntity
      cl(Req(GET, s"$iamBase/acls/$nonExistent", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.headOption.map(_._rev).getOrElse(0)

        cl(Req(PATCH, s"$iamBase/acls/$nonExistent?rev=$rev", headersServiceAccount, json))
          .mapResp(_.status shouldEqual StatusCodes.Created)
      }

    }

    "fail when organization does not exist" in {
      cl(Req(PUT, s"$adminBase/orgs/$nonExistent?rev=1", headersJsonUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.NotFound
        json shouldEqual jsonContentOf("/admin/errors/not-exists.json", Map(quote("{orgId}") -> nonExistent))
      }
    }

    "succeed and fetch revisions" in {

      val updatedName = s"$id organization update 1"
      val update      = orgReqEntity(updatedName)

      cl(Req(PUT, s"$adminBase/orgs/$id?rev=1", headersJsonUser, update)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeMetadata() shouldEqual createRespJson(id, 2L, "orgs", "Organization")
      }

      val updatedName2 = s"$id organization update 2"
      val update2      = orgReqEntity(updatedName2)

      cl(Req(PUT, s"$adminBase/orgs/$id?rev=2", headersJsonUser, update2)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeMetadata() shouldEqual createRespJson(id, 3L, "orgs", "Organization")

      }

      cl(Req(uri = s"$adminBase/orgs/$id", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, updatedName2, 3, id)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=3", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, updatedName2, 3L, id)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=2", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, updatedName, 2L, id)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=1", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, s"$id organization", 1L, id)
      }
    }

  }

  "deprecating an organization" should {
    val id     = genId()
    val name   = genString()
    val create = orgReqEntity(name)

    "add orgs/create permissions for user" in {
      val json = jsonContentOf(
        "/iam/add.json",
        replSub + (quote("{perms}") -> "organizations/create")
      ).toEntity

      cl(Req(PATCH, s"$iamBase/acls/$id?rev=0", headersServiceAccount, json))
        .mapResp(_.status shouldEqual StatusCodes.Created)

    }

    "create the organization" in {
      cl(Req(PUT, s"$adminBase/orgs/$id", headersJsonUser, create)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json.removeMetadata() shouldEqual createRespJson(id, 1L, "orgs", "Organization")
      }
    }

    "fail when wrong revision is provided" in {

      cl(Req(DELETE, s"$adminBase/orgs/$id?rev=4", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Conflict
        json shouldEqual jsonContentOf("/admin/errors/org-incorrect-revision.json")
      }
    }

    "fail when revision is not provided" in {
      cl(Req(DELETE, s"$adminBase/orgs/$id", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/admin/errors/rev-not-provided.json")
      }
    }

    "succeed if organization exists" in {
      cl(Req(DELETE, s"$adminBase/orgs/$id?rev=1", headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json.removeMetadata() shouldEqual createRespJson(id, 2L, "orgs", "Organization", true)
      }

      cl(Req(uri = s"$adminBase/orgs/$id", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, name, 2L, id, true)

      }

      cl(Req(uri = s"$adminBase/orgs/$id?rev=1", headers = headersJsonUser)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        validateAdminResource(json, "Organization", "orgs", id, name, 1L, id)

      }
    }
  }
}
