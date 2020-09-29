package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.Resources
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity
import ch.epfl.bluebrain.nexus.tests.iam.types.{Permission, Permissions}
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertion

class PermissionDsl(implicit cl: UntypedHttpClient[Task], materializer: Materializer) extends Resources {

  def permissionsMap(permissions: Iterable[Permission]) =
    Map(
      quote("{perms}") -> permissions.map { _.value }.mkString("\",\"")
    )

  def addPermissions(list: Permission*): Task[Assertion] =
    cl.get[Permissions]("/permissions", Identity.ServiceAccount) { (permissions, response) =>
      response.status shouldEqual StatusCodes.OK
      val body = jsonContentOf(
        "/iam/permissions/append.json",
        permissionsMap(list)
      )
      if (!list.toSet.subsetOf(permissions.permissions)) {
        cl.patch[Json](s"/permissions?rev=${permissions._rev}", body, Identity.ServiceAccount) { (_, response) =>
            response.status shouldEqual StatusCodes.OK
          }
          .runSyncUnsafe()
      } else {
        succeed
      }

    }

}
