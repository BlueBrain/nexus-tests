package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.Randomness
import ch.epfl.bluebrain.nexus.tests.Tags.{IamTag, PermissionsTag}
import ch.epfl.bluebrain.nexus.tests.iam.types.Permissions
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec}
import io.circe.Json

class PermissionsSpec extends NewBaseSpec
  with Randomness {

  "manage permissions" should {
    val permission1 = s"${genString(8)}/${genString(8)}"
    val permission2 = s"${genString(8)}/${genString(8)}"
    val minimumPermissions = Set(
      "acls/read",
      "acls/write",
      "events/read",
      "files/write",
      "organizations/create",
      "organizations/read",
      "organizations/write",
      "permissions/read",
      "permissions/write",
      "projects/create",
      "projects/read",
      "projects/write",
      "realms/read",
      "realms/write",
      "resolvers/write",
      "resources/read",
      "resources/write",
      "schemas/write",
      "views/query",
      "views/write",
      "storages/write",
      "archives/write"
    )

    "clear permissions" taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) {
        (permissions, response) =>
          response.status shouldEqual StatusCodes.OK
          if (permissions.permissions == minimumPermissions)
            succeed
          else
            cl.delete[Json](s"/permissions?rev=${permissions._rev}", Identity.ServiceAccount){
              (_, response) =>
                response.status shouldEqual StatusCodes.OK
        }
      }
    }

    "add permissions"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) {
        (permissions, response) =>
          response.status shouldEqual StatusCodes.OK
          val body = jsonContentOf(
            "/iam/permissions/append.json",
            Map(
              quote("{perms}") -> List(permission1, permission2).mkString("\",\"")
            )
          )
          cl.patch[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) {
            (_, response) => response.status shouldEqual StatusCodes.OK
          }
      }
    }

    "check added permissions"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual minimumPermissions + permission1 + permission2
      }
    }

    "subtract permissions"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        val body = jsonContentOf(
          "/iam/permissions/subtract.json",
          Map(
            quote("{perms}") -> permission2
          )
        )
        cl.patch[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.OK
        }
      }
    }

    "check subtracted permissions"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual minimumPermissions + permission1
      }
    }

    "replace permissions"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        val body =
          jsonContentOf(
            "/iam/permissions/replace.json",
            Map(
              quote("{perms}") -> (minimumPermissions + permission1 + permission2).mkString("\",\"")
            )
          )
        cl.put[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.OK
        }
      }
    }

    "check replaced permissions"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual minimumPermissions + permission1 + permission2
      }
    }

    "reject subtracting minimal permission"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        val body = jsonContentOf(
          "/iam/permissions/subtract.json",
          Map(
            quote("{perms}") -> minimumPermissions.head
          )
        )
        cl.patch[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.BadRequest
        }
      }
    }

    "reject replacing minimal permission"  taggedAs (IamTag, PermissionsTag) in {
      cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
        response.status shouldEqual StatusCodes.OK
        val body = jsonContentOf(
          "/iam/permissions/replace.json",
          Map(
            quote("{perms}") -> minimumPermissions.subsets(1).next().mkString("\",\"")
          )
        )
        cl.put[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.BadRequest
        }
      }
    }
  }
}
