package ch.epfl.bluebrain.nexus.tests

import java.util.regex.Pattern.quote

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import ch.epfl.bluebrain.nexus.commons.http.HttpClient
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.Resources
import ch.epfl.bluebrain.nexus.tests.config.TestConfig
import ch.epfl.bluebrain.nexus.tests.config.TestConfig.RemoteDeltaConfig
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclEntry, AclListing, AclUser}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, BeforeAndAfterAll}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._

trait NewBaseSpec extends AnyWordSpecLike
  with BeforeAndAfterAll
  with Resources
  with ScalatestRouteTest
  with ScalaFutures
  with Matchers {

  private[tests] val config: TestConfig = TestConfig.load(ConfigFactory.load())

  private[tests] val realmLabel = "internal"

  private[tests] implicit val cl: UntypedHttpClient[Task] = HttpClient.untyped[Task]

  private def toAuthorizationHeader(token: String) =
    Authorization(
      HttpCredentials.createOAuth2BearerToken(token)
    )

  // Initializing the requirements according to the configuration
  val (baseUrl: String, userTokens: Map[Identity, Authorization]) =
    config match {
      case r: RemoteDeltaConfig =>
        // Urls and tokens are provided by the configuration
        (
          r.deltaUri,
          r.userTokens.map { case (k, v) =>
            User(k, "no_password") -> toAuthorizationHeader(v)
          } ++ Map(Identity.ServiceAccount -> toAuthorizationHeader(r.serviceAccountToken))
        )
      case _ =>
        // Using docker-compose, urls are available as system-properties
        // And we retrieve tokens from the Keycloak that has been started
        (
          s"http://${System.getProperty("delta:8080")}/v1",
          (
            Identity.users.map { u =>
              u -> toAuthorizationHeader(Keycloak.userToken(u, Identity.ServiceAccount))
            } ++ Map(Identity.ServiceAccount -> toAuthorizationHeader(Keycloak.serviceAccountToken(Identity.ServiceAccount)))
          ).toMap
        )
    }

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(config.patience, 300.millis)

  override def beforeAll(): Unit = {
    super.beforeAll()
    ensureRealmExists
    cleanAcls
    ()
  }

  def cleanAcls: Assertion =
    cl.get[AclListing](s"/acls/*/*?ancestors=true&self=false", Identity.ServiceAccount) {
      (acls, response) =>
        response.status shouldEqual StatusCodes.OK

        val permissions = acls._results
          .map { acls =>
            val userAcls = acls.acl.filter {
              case AclEntry(AclUser(`realmLabel`, Identity.ServiceAccount.id), _) => true
              case _                                                       => false
            }
            acls.copy(acl = userAcls)
          }
          .filter(_.acl.nonEmpty)

        permissions.foreach { acl =>
          val body = jsonContentOf(
            "/iam/subtract-permissions.json",
              Map(
                quote("{sub}") -> Identity.ServiceAccount.id,
                quote("{perms}") -> acl.acl.head.permissions.mkString("\",\"")
              )
            )
          cl.patch[Json](s"/acls${acl._path}?rev=${acl._rev}", body, Identity.ServiceAccount) {
            (_, response) => response.status shouldEqual StatusCodes.OK
          }
        }
        succeed
    }

  def ensureRealmExists: Assertion = {
    val body =
      jsonContentOf(
        "/iam/realms/create.json",
        Map(
          quote("{realm}") -> config.realm
        )
      )

    cl.get[Json](s"/realms/$realmLabel", Identity.Anonymous) {
      (json, response) =>
        val rev = json.hcursor.downField("_rev")
          .as[Int]
          .toOption
        Some(response.status) should contain oneOf (StatusCodes.OK, StatusCodes.NotFound)
        rev match {
          case Some(r) =>
            cl.put[Json](s"/realms/$realmLabel?rev=$r", body, Identity.Anonymous) {
              (_, response) =>
                response.status shouldEqual StatusCodes.OK
            }
          case None =>
            cl.put[Json](s"/realms/$realmLabel", body, Identity.Anonymous) {
              (_, response) =>
                response.status shouldEqual StatusCodes.Created
            }
        }
    }
  }

  private[tests] implicit class HttpClientOps(val httpClient: UntypedHttpClient[Task]) {

    def post[A](url: String, body: Json, identity: Identity)
               (assertResponse: (A, HttpResponse) => Assertion)
               (implicit um: FromEntityUnmarshaller[A]): Assertion =
      request(POST, url, Some(body), identity)(assertResponse)

    def put[A](url: String, body: Json, identity: Identity)
              (assertResponse: (A, HttpResponse) => Assertion)
              (implicit um: FromEntityUnmarshaller[A]): Assertion =
      request(PUT, url, Some(body), identity)(assertResponse)

    def patch[A](url: String, body: Json, identity: Identity)
              (assertResponse: (A, HttpResponse) => Assertion)
              (implicit um: FromEntityUnmarshaller[A]): Assertion =
      request(PATCH, url, Some(body), identity)(assertResponse)

    def get[A](url: String, identity: Identity)
              (assertResponse: (A, HttpResponse) => Assertion)
              (implicit um: FromEntityUnmarshaller[A]): Assertion =
      request(GET, url, None, identity)(assertResponse)

    def delete[A](url: String, identity: Identity)
                 (assertResponse: (A, HttpResponse) => Assertion)
                 (implicit um: FromEntityUnmarshaller[A]): Assertion =
      request(DELETE, url, None, identity)(assertResponse)

    def request[A](method: HttpMethod,
                   url: String,
                   body: Option[Json],
                   identity: Identity)
                  (assertResponse: (A, HttpResponse) => Assertion)
                  (implicit um: FromEntityUnmarshaller[A]): Assertion =
      httpClient(
        HttpRequest(
          method = method,
          uri = s"$baseUrl$url",
          headers = userTokens.get(identity).fold(List.empty[Authorization])(List(_)),
          entity = body.fold(HttpEntity.Empty)
          (j => HttpEntity(ContentTypes.`application/json`, j.noSpaces))
        )
      ).flatMap { res =>
        Task.deferFuture {
          um(res.entity)(global, materializer)
        }.map {
          assertResponse(_, res)
        }
      }.runSyncUnsafe()

  }
}

object NewBaseSpec {

}
