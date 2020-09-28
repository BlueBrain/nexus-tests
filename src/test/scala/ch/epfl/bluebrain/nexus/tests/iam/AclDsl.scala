package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.Materializer
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity
import ch.epfl.bluebrain.nexus.tests.Identity.Authenticated
import ch.epfl.bluebrain.nexus.tests.Optics.error
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclEntry, AclListing, Permission, User}
import com.typesafe.scalalogging.Logger
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, OptionValues}

class AclDsl(implicit cl: UntypedHttpClient[Task],
             materializer: Materializer) extends Randomness with OptionValues with Resources with Matchers {

  private val logger = Logger[this.type]

  def addPermission(path: String,
                    target: Authenticated,
                    permission: Permission): Task[Assertion] =
    addPermissions(path, target, Set(permission))

  def addPermissions(path: String,
                     target: Authenticated,
                     permissions: Set[Permission]): Task[Assertion] = {
    val permissionsMap = Map(
      quote("{realm}") -> target.realm.name,
      quote("{sub}") -> target.name,
      quote("{perms}") -> permissions.map(_.value).mkString("""","""")
    )

    val json = jsonContentOf(
      "/iam/add.json",
      permissionsMap
    )

    addPermissions(path, json, target.name)
  }

  def addPermissionAnonymous(path: String,
                             permission: Permission): Task[Assertion] =
    addPermissionsAnonymous(path, Set(permission))

  def addPermissionsAnonymous(path: String,
                              permissions: Set[Permission]): Task[Assertion] = {
    val json = jsonContentOf("/iam/add_annon.json",
      Map(
        quote("{perms}") -> permissions.map(_.value).mkString("""","""")
      )
    )

    addPermissions(path, json, "Anonymous")
  }

  def addPermissions(path: String,
                     payload: Json,
                     targetName: String): Task[Assertion] = {
    path should not startWith "/acls"
    logger.info(s"Addings permissions to $path for $targetName")

    def assertResponse(json: Json, response: HttpResponse) =
      response.status match {
        case StatusCodes.Created | StatusCodes.OK =>
          logger.info(s"Permissions has been successfully added for $targetName on $path")
          succeed
        case StatusCodes.BadRequest =>
          val errorType = error.`@type`.getOption(json)
          logger.warn(
            s"We got a bad request when adding permissions for $targetName on $path with error type $errorType"
          )
          errorType.value shouldBe "NothingToBeUpdated"
        case s => fail(s"We were not expecting $s when setting acls on $path for $targetName")
      }

    cl.get[AclListing](s"/acls$path", Identity.ServiceAccount) { (acls, response) =>
      {
        response.status shouldEqual StatusCodes.OK
        val rev = acls._results.headOption
        rev match {
          case Some(r) =>
            cl.patch[Json](s"/acls$path?rev=${r._rev}", payload, Identity.ServiceAccount) {
              assertResponse
            }
          case None    =>
            cl.put[Json](s"/acls$path", payload, Identity.ServiceAccount) {
              assertResponse
            }
        }
      }.runSyncUnsafe()
    }
  }

  def cleanAcls(target: Authenticated): Task[Assertion] =
    cl.get[AclListing](s"/acls/*/*?ancestors=true&self=false", Identity.ServiceAccount)
      { (acls, response) =>
        response.status shouldEqual StatusCodes.OK

        val permissions = acls._results
          .map { acls =>
            val userAcls = acls.acl.filter {
              case AclEntry(User(_, name), _) if name == target.name => true
              case _                                                 => false
            }
            acls.copy(acl = userAcls)
          }
          .filter(_.acl.nonEmpty)

        permissions.traverse { acl =>
          val payload = jsonContentOf(
            "/iam/subtract-permissions.json",
            Map(
              quote("{realm}") -> target.realm.name,
              quote("{sub}") -> target.name,
              quote("{perms}") -> acl.acl.head.permissions.map(_.value).mkString("""","""")
            )
          )
          cl.patch[Json](s"/acls${acl._path}?rev=${acl._rev}", payload, Identity.ServiceAccount) {
            (_, response ) => response.status shouldEqual StatusCodes.OK
          }
        }.map(_ => succeed).runSyncUnsafe()
      }

  def deletePermission(path: String,
                       target: Authenticated,
                       permission: Permission): Task[Assertion] =
    deletePermissions(path, target, Set(permission))

  def deletePermissions(path: String,
                        target: Authenticated,
                        permissions: Set[Permission],
                       ): Task[Assertion] = {
    path should not startWith "/acls"
    cl.get[Json](s"/acls$path", Identity.ServiceAccount) { (json, response) =>
    {
        response.status shouldEqual StatusCodes.OK
        val acls = json
          .as[AclListing]
          .getOrElse(throw new RuntimeException(s"Couldn't decode ${json.noSpaces} to AclListing"))

        deletePermissions(
          path,
          target,
          acls._results.head._rev,
          permissions
        )
      }.runSyncUnsafe()
    }
  }

  def deletePermission(path: String,
                        target: Authenticated,
                        revision: Long,
                        permission: Permission): Task[Assertion] = {
    deletePermissions(path, target, revision, Set(permission))
  }

  def deletePermissions(path: String,
                       target: Authenticated,
                       revision: Long,
                       permissions: Set[Permission]): Task[Assertion] = {
    val body = jsonContentOf(
      "/iam/subtract-permissions.json",
      Map(
        quote("{realm}") -> target.realm.name,
        quote("{sub}") -> target.name,
        quote("{perms}") -> permissions.map(_.value).mkString("""","""")
      )
    )
    cl.patch[Json](s"/acls$path?rev=$revision", body, Identity.ServiceAccount) {
      (_, response) => response.status shouldEqual StatusCodes.OK
    }
  }

  def checkAdminAcls(path: String, authenticated: Authenticated): Task[Assertion] = {
    logger.info(s"Gettings acls for $path using ${authenticated.name}")
    cl.get[AclListing](s"/acls$path", authenticated) { (acls, response) =>
      response.status shouldEqual StatusCodes.OK
      val acl = acls._results.headOption.value
      val entry = acl.acl.headOption.value
      entry.permissions shouldEqual Permission.adminPermissions
    }
  }

}
