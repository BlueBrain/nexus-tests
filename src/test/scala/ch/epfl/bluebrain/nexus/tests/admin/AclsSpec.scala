package ch.epfl.bluebrain.nexus.tests.admin

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods.{DELETE, PATCH, PUT}
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

class AclsSpec extends BaseSpec with Inspectors with CancelAfterFailure with Eventually {

  "checking ACLs" should {

    "add permissions for users" in {
      val json =
        jsonContentOf("/iam/add.json",
                      replSub + (quote("{perms}") -> """projects/create","projects/read","projects/write""")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }

    }

    "delete some permissions for user" in {
      val entity = jsonContentOf("/iam/patch-single.json", replSub + (quote("{perms}") -> "projects/read")).toEntity
      cl(Req(PATCH, s"$iamBase/acls/", headersGroup, entity)).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.OK
        json should equalIgnoreArrayOrder(
          jsonContentOf("/iam/patch-response-perms.json",
                        replSub ++ resourceIamCtx + (quote("{perms}") -> """projects/create","projects/write""")))
      }
    }

    "fail if the projects/read permissions are missing" in {
      eventually {
        cl(Req(uri = s"$adminBase/projects/${genId()}/${genId()}/acls", headers = headersUser)).mapJson {
          (json, result) =>
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

    "add organization/create permissions for user" in {
      val json = jsonContentOf("/iam/add.json", replSub + (quote("{perms}") -> "orgs/create")).toEntity
      cl(Req(PUT, s"$iamBase/acls/", headersGroup, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.OK
        result.entity.isKnownEmpty() shouldEqual true
      }
    }

    "return not found when project does not exist" in {
      eventually {
        cl(Req(uri = s"$adminBase/projects/${genId()}/${genId()}/acls", headers = headersUser)).mapJson {
          (json, result) =>
            result.status shouldEqual StatusCodes.NotFound
            json shouldEqual jsonContentOf("/admin/errors/not-exists.json", errorCtx)
        }
      }
    }

    "return underlying ACLs for a project with parents=true query param" in {
      val projId = genId()
      val orgId  = genId()

      val id = s"$orgId/$projId"
      eventually {
        cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(orgId, 1L, "orgs")
        }
      }
      eventually {
        cl(Req(PUT, s"$adminBase/projects/$id", headersUser, projectReqEntity())).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.Created
          json shouldEqual createRespJson(id, 1L)
        }
      }
      eventually {
        cl(Req(uri = s"$adminBase/projects/$id/acls?parents=true", headers = headersUser)).mapJson { (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json should equalIgnoreArrayOrder(
            jsonContentOf(
              "/iam/project-perms-response.json",
              resourceIamCtx ++ replSub + (quote("{perms}") -> """projects/create","projects/write","orgs/create","projects/read""", quote(
                "{path}")                                   -> "")
            ))
        }
      }
    }

    "return empty ACLs for path without them" in {
      val projId = genId()
      val orgId  = genId()

      val id = s"$orgId/$projId"
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(orgId, 1L, "orgs")
      }
      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, projectReqEntity())).mapJson { (json, result) =>
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
      val projId = genId()
      val orgId  = genId()

      val id = s"$orgId/$projId"
      cl(Req(PUT, s"$adminBase/orgs/$orgId", headersUser, orgReqEntity())).mapJson { (json, result) =>
        result.status shouldEqual StatusCodes.Created
        json shouldEqual createRespJson(orgId, 1L, "orgs")
      }
      cl(Req(PUT, s"$adminBase/projects/$id", headersUser, projectReqEntity())).mapJson { (json, result) =>
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
          json should (equal(
            jsonContentOf("/iam/project-perms-response.json",
                          resourceIamCtx ++ replSub + (quote("{perms}") -> s"""$permission","$permission2""", quote(
                            "{path}")                                   -> id)))
            or equal(
              jsonContentOf("/iam/project-perms-response.json",
                            resourceIamCtx ++ replSub + (quote("{perms}") -> s"""$permission2","$permission""", quote(
                              "{path}")                                   -> id))))
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
