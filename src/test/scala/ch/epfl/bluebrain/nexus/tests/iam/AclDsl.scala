package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.DeltaHttpClient._
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.Identity
import ch.epfl.bluebrain.nexus.tests.Identity.Authenticated
import ch.epfl.bluebrain.nexus.tests.Optics.error
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclListing, Permission}
import com.typesafe.scalalogging.Logger
import monix.execution.Scheduler.Implicits.global
import io.circe.Json
import monix.bio.Task
import org.scalatest.{Assertion, OptionValues}
import org.scalatest.matchers.should.Matchers

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
    path should not startWith "/acls"

    val permissionsMap = Map(
      quote("{realm}") -> target.realm.name,
      quote("{sub}") -> target.name,
      quote("{perms}") -> permissions.map(_.value).mkString("""","""")
    )

    val json = jsonContentOf(
      "/iam/add.json",
      permissionsMap
    )

    def assertResponse(json: Json, response: HttpResponse) =
      response.status match {
        case StatusCodes.Created | StatusCodes.OK =>
          logger.info(s"Permissions has been successfully added for ${target.name} on $path")
          succeed
        case StatusCodes.BadRequest =>
          val errorType = error.`@type`.getOption(json)
          logger.warn(
            s"We got a bad request when adding permissions for ${target.name} on $path with error type $errorType"
          )
          errorType.value shouldBe "NothingToBeUpdated"
        case s => fail(s"We were not expecting $s when setting acls on $path for realm ${target.realm.name} for ${target.name}")
      }

    cl.get[AclListing](s"/acls$path", Identity.ServiceAccount) { (acls, response) =>
      {
        response.status shouldEqual StatusCodes.OK
        val rev = acls._results.headOption
        rev match {
          case Some(r) =>
            cl.patch[Json](s"/acls$path?rev=${r._rev}", json, Identity.ServiceAccount) {
              assertResponse
            }
          case None    =>
            cl.put[Json](s"/acls$path", json, Identity.ServiceAccount) {
              assertResponse
            }
        }
      }.runSyncUnsafe()
    }
  }

}
