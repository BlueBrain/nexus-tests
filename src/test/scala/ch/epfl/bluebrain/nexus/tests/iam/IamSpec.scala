package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpRequest => Req}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.Randomness
import ch.epfl.bluebrain.nexus.tests.BaseSpec
import ch.epfl.bluebrain.nexus.tests.Tags.ToMigrateTag
import ch.epfl.bluebrain.nexus.tests.iam.types._
import org.scalatest.concurrent.Eventually
import org.scalatest.{CancelAfterFailure, Inspectors}

class IamSpec extends BaseSpec with Inspectors with CancelAfterFailure with Eventually with Randomness {

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

    "clear permissions" taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        if (permissions.permissions == minimumPermissions)
          succeed
        else
          cl(Req(DELETE, s"$iamBase/permissions?rev=${permissions._rev}", headersServiceAccount))
            .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }
    "add permissions"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        val body = jsonContentOf(
          "/iam/permissions/append.json",
          Map(
            quote("{perms}") -> List(permission1, permission2).mkString("\",\"")
          )
        ).toEntity
        cl(Req(PATCH, s"$iamBase/permissions?rev=${permissions._rev}", headersServiceAccount, body))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "check added permissions"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual minimumPermissions + permission1 + permission2
      }
    }

    "subtract permissions"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        val body = jsonContentOf(
          "/iam/permissions/subtract.json",
          Map(
            quote("{perms}") -> permission2
          )
        ).toEntity
        cl(Req(PATCH, s"$iamBase/permissions?rev=${permissions._rev}", headersServiceAccount, body))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }

    }
    "check subtracted permissions"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual minimumPermissions + permission1
      }
    }

    "replace permissions"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        val body =
          jsonContentOf(
            "/iam/permissions/replace.json",
            Map(
              quote("{perms}") -> (minimumPermissions + permission1 + permission2).mkString("\",\"")
            )
          ).toEntity
        cl(Req(PUT, s"$iamBase/permissions?rev=${permissions._rev}", headersServiceAccount, body))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "check replaced permissions"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        permissions.permissions shouldEqual minimumPermissions + permission1 + permission2
      }
    }

    "reject subtracting minimal permission"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        val body = jsonContentOf(
          "/iam/permissions/subtract.json",
          Map(
            quote("{perms}") -> minimumPermissions.head
          )
        ).toEntity
        cl(Req(PATCH, s"$iamBase/permissions?rev=${permissions._rev}", headersServiceAccount, body))
          .mapResp(_.status shouldEqual StatusCodes.BadRequest)
      }
    }

    "reject replacing minimal permission"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/permissions", headersServiceAccount)).mapDecoded[Permissions] { (permissions, result) =>
        result.status shouldEqual StatusCodes.OK
        val body = jsonContentOf(
          "/iam/permissions/replace.json",
          Map(
            quote("{perms}") -> minimumPermissions.subsets(1).next().mkString("\",\"")
          )
        ).toEntity
        cl(Req(PUT, s"$iamBase/permissions?rev=${permissions._rev}", headersServiceAccount, body))
          .mapResp(_.status shouldEqual StatusCodes.BadRequest)
      }
    }
  }
  "manage acls" should {

    val orgPath1     = genString()
    val orgPath2     = genString()
    val projectPath1 = genString()
    val projectPath2 = genString()

    "add permissions for user on /"  taggedAs ToMigrateTag in {
      ensureRealmExists
      val json =
        jsonContentOf(
          "/iam/add.json",
          replSub + (quote("{perms}") -> """projects/create","projects/read","projects/write""")
        ).toEntity
      cl(Req(GET, s"$iamBase/acls/", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersServiceAccount, json))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      }
    }

    "fetch permissions for user"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/acls/?self=false", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        acls._results.head.acl
          .find {
            case AclEntry(AclUser(_, config.iam.`testUserSub`), _) => true
            case _                                              => false
          }
          .value
          .permissions shouldEqual Set("projects/create", "projects/read", "projects/write")
      }
    }

    "delete some permissions for user"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/acls/", headersServiceAccount)).mapJson { (js, result) =>
        result.status shouldEqual StatusCodes.OK
        val acls = js
          .as[AclListing]
          .getOrElse(throw new RuntimeException(s"Couldn't decode ${js.noSpaces} to AclListing"))

        val rev = acls._results.head._rev
        val entity =
          jsonContentOf("/iam/subtract-permissions.json", replSub + (quote("{perms}") -> "projects/write")).toEntity
        cl(Req(PATCH, s"$iamBase/acls/?rev=$rev", headersServiceAccount, entity)).mapResp { result =>
          result.status shouldEqual StatusCodes.OK
        }
      }
    }

    "check if permissions were removed"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/acls/?self=false", headersServiceAccount)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        acls._results.head.acl
          .find {
            case AclEntry(AclUser(`realmLabel`, config.iam.testUserSub), _) => true
            case _                                                       => false
          }
          .value
          .permissions shouldEqual Set("projects/create", "projects/read")
      }
    }

    "add permissions for user on paths with depth1"  taggedAs ToMigrateTag in {
      val json =
        jsonContentOf(
          "/iam/add.json",
          replSub + (quote("{perms}") -> """projects/create","projects/read","projects/write""")
        ).toEntity

      cl(Req(PUT, s"$iamBase/acls/$orgPath1", headersServiceAccount, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.Created
      }
      cl(Req(PUT, s"$iamBase/acls/$orgPath2", headersServiceAccount, json)).mapResp { result =>
        result.status shouldEqual StatusCodes.Created
      }
    }

    "add permissions for user on /orgpath/projectpath1 and /orgpath/projectpath2"  taggedAs ToMigrateTag in {
      val body =
        jsonContentOf(
          "/iam/add.json",
          replSub + (quote("{perms}") -> """projects/create","projects/read","projects/write""")
        ).toEntity
      cl(Req(PATCH, s"$iamBase/acls/$orgPath1/$projectPath1", headersServiceAccount, body))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PATCH, s"$iamBase/acls/$orgPath1/$projectPath2", headersServiceAccount, body))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PATCH, s"$iamBase/acls/$orgPath2/$projectPath1", headersServiceAccount, body))
        .mapResp(_.status shouldEqual StatusCodes.Created)
      cl(Req(PATCH, s"$iamBase/acls/$orgPath2/$projectPath2", headersServiceAccount, body))
        .mapResp(_.status shouldEqual StatusCodes.Created)
    }

    "list permissions on /*/*"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/acls/*/*", headersJsonUser)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        acls._total should be >= 4L
        val expectedPermissions = Set("projects/create", "projects/read", "projects/write")
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath1")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath2")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath2/$projectPath1")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath2")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
      }
    }

    "list permissions on /orgpath1/*"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/acls/$orgPath1/*", headersJsonUser)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        acls._total shouldEqual 2
        val expectedPermissions = Set("projects/create", "projects/read", "projects/write")
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath1")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath2")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions

      }
    }

    "list permissions on /*/* with ancestors"  taggedAs ToMigrateTag in {
      cl(Req(GET, s"$iamBase/acls/*/*?ancestors=true", headersJsonUser)).mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        val expectedPermissions = Set("projects/create", "projects/read", "projects/write")
        acls._results
          .find(_._path == "/")
          .value
          .acl
          .find {
            case AclEntry(AclUser(`realmLabel`, config.iam.testUserSub), _) => true
            case _                                                       => false
          }
          .value
          .permissions shouldEqual Set("projects/create", "projects/read")
        acls._results
          .find(_._path == s"/$orgPath1")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath2")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath1")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath2")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath2/$projectPath1")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
        acls._results
          .find(_._path == s"/$orgPath1/$projectPath2")
          .value
          .acl
          .head
          .permissions shouldEqual expectedPermissions
      }
    }
  }

}
