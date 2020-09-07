package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.Randomness
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Tags.{AclsTag, IamTag}
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclEntry, AclListing, User}
import ch.epfl.bluebrain.nexus.tests.{Identity, Keycloak, NewBaseSpec}
import io.circe.Json
import org.scalatest.OptionValues

class AclsSpec extends NewBaseSpec
  with OptionValues
  with Randomness {

  private val testRealm   = "acls" + genString()
  private val testClient = Identity.ClientCredentials(genString(), genString())
  private val Marge = UserCredentials(genString(), genString())
  private val Homer = UserCredentials(genString(), genString())

  override def beforeAll(): Unit = {
    super.beforeAll()
    Keycloak.importRealm(testRealm, testClient, Marge :: Homer :: Nil) shouldEqual StatusCodes.Created
    authenticateClient(testRealm, testClient)
    authenticateUser(testRealm, Marge, testClient)
    initRealm(testRealm, Identity.ServiceAccount)
    ()
  }

  "manage acls" should {
    val orgPath1 = genString()
    val orgPath2 = genString()
    val orgs = orgPath1 :: orgPath2 :: Nil
    val projectPath1 = genString()
    val projectPath2 = genString()
    val projects = projectPath1 :: projectPath2 :: Nil

    val crossProduct =
      for {
        org <- orgs
        project <- projects
      } yield {
        (org, project)
      }

    def permissionsMap(permissions: String) =
      Map(
        quote("{realm}") -> testRealm,
        quote("{sub}") -> Marge.name,
        quote("{perms}") -> permissions
      )

    "add permissions for user on /"  taggedAs (IamTag, AclsTag) in {
      val json = jsonContentOf(
        "/iam/add.json",
        permissionsMap("""projects/create","projects/read","projects/write""")
      )
      cl.get[AclListing]("/acls/", Identity.ServiceAccount) { (acls, response) =>
        response.status shouldEqual StatusCodes.OK
        val rev = acls._results.head._rev

        cl.patch[Json](s"/acls/?rev=$rev", json, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.OK
        }
      }
    }

    "fetch permissions for user"  taggedAs (IamTag, AclsTag) in {
      cl.get[AclListing]("/acls/?self=false", Identity.ServiceAccount) { (acls, result) =>
        result.status shouldEqual StatusCodes.OK
        acls._results.head.acl
          .find {
            case AclEntry(User(_, Marge.name), _) => true
            case _                                 => false
          }
          .value
          .permissions shouldEqual Set("projects/create", "projects/read", "projects/write")
      }
    }

    "delete some permissions for user"  taggedAs (IamTag, AclsTag) in {
      cl.get[Json](s"/acls/", Identity.ServiceAccount) { (json, response) =>
        response.status shouldEqual StatusCodes.OK
        val acls = json
          .as[AclListing]
          .getOrElse(throw new RuntimeException(s"Couldn't decode ${json.noSpaces} to AclListing"))

        val rev = acls._results.head._rev
        val body = jsonContentOf(
          "/iam/subtract-permissions.json",
          permissionsMap("projects/write")
        )
        cl.patch[Json](s"/acls/?rev=$rev", body, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.OK
        }
      }
    }

    "check if permissions were removed"  taggedAs (IamTag, AclsTag) in {
      cl.get[AclListing]("/acls/?self=false", Identity.ServiceAccount) { (acls, response) =>
        response.status shouldEqual StatusCodes.OK
        acls._results.head.acl
          .find {
            case AclEntry(User(`testRealm`, Marge.name), _) => true
            case _                                          => false
          }
          .value
          .permissions shouldEqual Set("projects/create", "projects/read")
      }
    }

    "add permissions for user on paths with depth1"  taggedAs (IamTag, AclsTag) in {
      val body = jsonContentOf(
        "/iam/add.json",
        permissionsMap("""projects/create","projects/read","projects/write""")
      )

      orgs.foreach { org =>
        cl.put[Json](s"/acls/$org", body, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    "add permissions for user on /orgpath/projectpath1 and /orgpath/projectpath2" taggedAs (IamTag, AclsTag) in {
      val body = jsonContentOf(
        "/iam/add.json",
        permissionsMap("""projects/create","projects/read","projects/write""")
      )

      crossProduct.foreach { case (org, project) =>
        cl.patch[Json](  s"/acls/$org/$project", body, Identity.ServiceAccount) {
          (_, response) => response.status shouldEqual StatusCodes.Created
        }
      }
    }

    def assertPermissions(acls:AclListing,
                         org: String,
                         project: String,
                         expectedPermissions: Set[String]) =
      acls._results
        .find(_._path == s"/$org/$project")
        .value
        .acl
        .head
        .permissions shouldEqual expectedPermissions


    "list permissions on /*/*"  taggedAs (IamTag, AclsTag) in {
      cl.get[AclListing]( "/acls/*/*", Marge) {
        (acls, response) =>
          response.status shouldEqual StatusCodes.OK
          val expectedPermissions = Set("projects/create", "projects/read", "projects/write")

          crossProduct.foreach { case (org, project) =>
            assertPermissions(acls, org, project, expectedPermissions)
          }
          succeed
      }
    }

    "list permissions on /orgpath1/*"  taggedAs (IamTag, AclsTag) in {
      cl.get[AclListing]( s"/acls/$orgPath1/*", Marge) {
        (acls, response) =>
          response.status shouldEqual StatusCodes.OK
          acls._total shouldEqual 2
          val expectedPermissions = Set("projects/create", "projects/read", "projects/write")
          projects.foreach { project =>
            assertPermissions(acls, orgPath1, project, expectedPermissions)
          }
          succeed
      }
    }

    "list permissions on /*/* with ancestors"  taggedAs (IamTag, AclsTag) in {
      cl.get[AclListing]( "/acls/*/*?ancestors=true", Marge) {
        (acls, response) =>
          response.status shouldEqual StatusCodes.OK
          val createRead = Set("projects/create", "projects/read")
          val createReadWrite = Set("projects/create", "projects/read", "projects/write")

          acls._results
            .find(_._path == "/")
            .value
            .acl
            .find {
              case AclEntry(User(`testRealm`, Marge.name), _) => true
              case _                                         => false
            }
            .value
            .permissions shouldEqual createRead

          orgs.foreach { org =>
            acls._results
              .find(_._path == s"/$org")
              .value
              .acl
              .head
              .permissions shouldEqual createReadWrite
          }

          crossProduct.foreach { case (org, project) =>
            assertPermissions(acls, org, project, createReadWrite)
          }

          succeed
      }
    }
  }
}
