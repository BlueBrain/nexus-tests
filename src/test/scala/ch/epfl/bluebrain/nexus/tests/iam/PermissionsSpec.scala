package ch.epfl.bluebrain.nexus.tests.iam

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Tags.{IamTag, PermissionsTag}
import ch.epfl.bluebrain.nexus.tests.iam.types.{Permission, Permissions}
import ch.epfl.bluebrain.nexus.tests.{BaseSpec, Identity}
import io.circe.Json
import monix.bio.Task

class PermissionsSpec extends BaseSpec {

  "manage permissions" should {
    val permission1 = Permission(genString(8), genString(8))
    val permission2 = Permission(genString(8), genString(8))

    "clear permissions" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        runTask {
          response.status shouldEqual StatusCodes.OK
          if (permissions.permissions == Permission.minimalPermissions)
            Task(succeed)
          else
            cl.delete[Json](s"/permissions?rev=${permissions._rev}", Identity.ServiceAccount) { (_, response) =>
              response.status shouldEqual StatusCodes.OK
            }
        }
      }
    }

    "add permissions" taggedAs (IamTag, PermissionsTag) in {
      permissionDsl.addPermissions(
        permission1,
        permission2
      )
    }

    "check added permissions" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual Permission.minimalPermissions + permission1 + permission2
      }
    }

    "subtract permissions" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        runTask {
          response.status shouldEqual StatusCodes.OK
          val body = jsonContentOf(
            "/iam/permissions/subtract.json",
            permissionDsl.permissionsMap(permission2 :: Nil)
          )
          cl.patch[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) { (_, response) =>
            response.status shouldEqual StatusCodes.OK
          }
        }
      }
    }

    "check subtracted permissions" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual Permission.minimalPermissions + permission1
      }
    }

    "replace permissions" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        runTask {
          response.status shouldEqual StatusCodes.OK
          val body =
            jsonContentOf(
              "/iam/permissions/replace.json",
              permissionDsl.permissionsMap(
                Permission.minimalPermissions + permission1 + permission2
              )
            )
          cl.put[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) { (_, response) =>
            response.status shouldEqual StatusCodes.OK
          }
        }
      }
    }

    "check replaced permissions" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual Permission.minimalPermissions + permission1 + permission2
      }
    }

    "reject subtracting minimal permission" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        runTask {
          response.status shouldEqual StatusCodes.OK
          val body = jsonContentOf(
            "/iam/permissions/subtract.json",
            permissionDsl.permissionsMap(
              Permission.minimalPermissions.take(1)
            )
          )
          cl.patch[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) { (_, response) =>
            response.status shouldEqual StatusCodes.BadRequest
          }
        }
      }
    }

    "reject replacing minimal permission" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        runTask {
          response.status shouldEqual StatusCodes.OK
          val body = jsonContentOf(
            "/iam/permissions/replace.json",
            permissionDsl.permissionsMap(
              Permission.minimalPermissions.subsets(1).next()
            )
          )
          cl.put[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) { (_, response) =>
            response.status shouldEqual StatusCodes.BadRequest
          }
        }
      }
    }
  }
}
