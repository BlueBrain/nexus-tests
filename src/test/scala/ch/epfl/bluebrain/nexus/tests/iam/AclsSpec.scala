package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.DeltaHttpClient._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Tags.{AclsTag, IamTag}
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclEntry, AclListing, Permission, User}
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec}
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import org.scalatest.OptionValues

class AclsSpec extends NewBaseSpec
  with OptionValues {

  private val testRealm   = "acls" + genString()
  private val testClient = Identity.ClientCredentials(genString(), genString())
  private val Marge = UserCredentials(genString(), genString())
  private val Homer = UserCredentials(genString(), genString())

  override def beforeAll(): Unit = {
    super.beforeAll()
    initRealm(
      testRealm,
      Identity.ServiceAccount,
      testClient,
      Marge :: Homer :: Nil
    ).runSyncUnsafe()
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

    val defaultPermissions = Permission.Projects.list.toSet
    val restrictedPermissions = defaultPermissions.filterNot(_ == Permission.Projects.Write)

    def permissionsMap(permissions: Permission*) =
      Map(
        quote("{realm}") -> testRealm,
        quote("{sub}") -> Marge.name,
        quote("{perms}") -> permissions.map(_.value).mkString("""","""")
      )

    "add permissions for user on /"  taggedAs (IamTag, AclsTag) in {
      addPermissions("/",
        Marge, testRealm,
        defaultPermissions
      ).runSyncUnsafe()
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
          .permissions shouldEqual defaultPermissions
      }.runSyncUnsafe()
    }

    "delete some permissions for user"  taggedAs (IamTag, AclsTag) in {
      cl.get[Json](s"/acls/", Identity.ServiceAccount) { (json, response) =>
        runTask {
          response.status shouldEqual StatusCodes.OK
          val acls = json
            .as[AclListing]
            .getOrElse(throw new RuntimeException(s"Couldn't decode ${json.noSpaces} to AclListing"))

          val rev = acls._results.head._rev
          val body = jsonContentOf(
            "/iam/subtract-permissions.json",
            permissionsMap(Permission.Projects.Write)
          )
          cl.patch[Json](s"/acls/?rev=$rev", body, Identity.ServiceAccount) {
            (_, response) => response.status shouldEqual StatusCodes.OK
          }
        }
      }.runSyncUnsafe()
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
          .permissions shouldEqual restrictedPermissions
      }.runSyncUnsafe()
    }

    "add permissions for user on paths with depth1"  taggedAs (IamTag, AclsTag) in {
      orgs.traverse { org =>
        addPermissions(
          s"/$org",
          Marge,
          testRealm,
          defaultPermissions
        )
      }.runSyncUnsafe()
    }

    "add permissions for user on /orgpath/projectpath1 and /orgpath/projectpath2" taggedAs (IamTag, AclsTag) in {
      crossProduct.traverse { case (org, project) =>
        addPermissions(
          s"/$org/$project",
          Marge,
          testRealm,
          defaultPermissions
        )
      }.runSyncUnsafe()
    }

    def assertPermissions(acls:AclListing,
                         org: String,
                         project: String,
                         expectedPermissions: Set[Permission]) =
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
          crossProduct.foreach { case (org, project) =>
            assertPermissions(acls, org, project, defaultPermissions)
          }
          succeed
      }.runSyncUnsafe()
    }

    "list permissions on /orgpath1/*"  taggedAs (IamTag, AclsTag) in {
      cl.get[AclListing]( s"/acls/$orgPath1/*", Marge) {
        (acls, response) =>
          response.status shouldEqual StatusCodes.OK
          acls._total shouldEqual 2
          projects.foreach { project =>
            assertPermissions(acls, orgPath1, project, defaultPermissions)
          }
          succeed
      }.runSyncUnsafe()
    }

    "list permissions on /*/* with ancestors"  taggedAs (IamTag, AclsTag) in {
      cl.get[AclListing]( "/acls/*/*?ancestors=true", Marge) {
        (acls, response) =>
          response.status shouldEqual StatusCodes.OK
          acls._results
            .find(_._path == "/")
            .value
            .acl
            .find {
              case AclEntry(User(`testRealm`, Marge.name), _) => true
              case _                                         => false
            }
            .value
            .permissions shouldEqual restrictedPermissions

          orgs.foreach { org =>
            acls._results
              .find(_._path == s"/$org")
              .value
              .acl
              .head
              .permissions shouldEqual defaultPermissions
          }

          crossProduct.foreach { case (org, project) =>
            assertPermissions(acls, org, project, defaultPermissions)
          }

          succeed
      }.runSyncUnsafe()
    }
  }
}
