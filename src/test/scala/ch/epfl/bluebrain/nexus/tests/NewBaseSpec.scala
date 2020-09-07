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
import ch.epfl.bluebrain.nexus.tests.Identity._
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.config.TestsConfig
import ch.epfl.bluebrain.nexus.tests.iam.PermissionsSpec
import ch.epfl.bluebrain.nexus.tests.iam.types.AclListing
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{Assertion, BeforeAndAfterAll}

import scala.collection.mutable
import scala.concurrent.duration._

trait NewBaseSpec extends AnyWordSpecLike
  with BeforeAndAfterAll
  with Resources
  with ScalatestRouteTest
  with ScalaFutures
  with Matchers {

  private val logger = Logger[this.type]

  private[tests] val config: TestsConfig = TestsConfig.load(ConfigFactory.load())

  private[tests] val internalRealm = "internal"

  val tokensMap: mutable.Map[Identity, Authorization]= collection.mutable.Map.empty[Identity, Authorization]

  private[tests] implicit val cl: UntypedHttpClient[Task] = HttpClient.untyped[Task]

  val baseUrl: Uri = Uri(s"http://${System.getProperty("delta:8080")}/v1")

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(config.patience, 300.millis)

  override def beforeAll(): Unit = {
    super.beforeAll()
    Keycloak.importRealm(internalRealm, Identity.ServiceAccount, Identity.Alice :: Identity.Bob :: Nil)
    authenticateUser(internalRealm, Identity.Alice, Identity.ServiceAccount)
    authenticateUser(internalRealm, Identity.Bob, Identity.ServiceAccount)
    authenticateClient(internalRealm, Identity.ServiceAccount)
    initRealm(internalRealm, Identity.Anonymous)
    initServiceAccountAcls()
    ()
  }

  private def toAuthorizationHeader(token: String) =
    Authorization(
      HttpCredentials.createOAuth2BearerToken(token)
    )

  private[tests] def authenticateUser(realm: String, user: UserCredentials, client: ClientCredentials) = {
    tokensMap.addOne(user -> toAuthorizationHeader(
      Keycloak.userToken(realm, user, client))
    )
  }

  private[tests] def authenticateClient(realm: String, client: ClientCredentials) = {
    tokensMap.addOne(client -> toAuthorizationHeader(
      Keycloak.serviceAccountToken(realm, client))
    )
  }

  // Gives all permissions on / to ServiceAccount
  private def initServiceAccountAcls() = {
    val json = jsonContentOf(
      "/iam/add.json",
      Map(
        quote("{realm}") -> internalRealm,
        quote("{sub}") -> Identity.ServiceAccount.name,
        quote("{perms}") -> PermissionsSpec.minimumPermissions.mkString("""","""")
      )
    )

    cl.get[AclListing]("/acls/", Identity.ServiceAccount) { (acls, response) =>
      response.status shouldEqual StatusCodes.OK
      val rev = acls._results.head._rev

      cl.patch[Json](s"/acls/?rev=$rev", json, Identity.ServiceAccount) {
        (json, response) =>
          response.status match {
            case StatusCodes.Created => succeed
            case StatusCodes.BadRequest =>
              error.`@type`.getOption(json) shouldBe Some("NothingToBeUpdated")
            case s => fail(s"We were not expecting $s when setting default acls for service account")
          }
      }
    }
  }

  // Init the given realm
  def initRealm(realm: String, identity: Identity): Assertion = {
    cl.get[Json](s"/realms/$realm", identity) {
      (json, response) =>
        response.status match {
          case StatusCodes.NotFound =>
            logger.info(s"Realm $realm is absent, we create it")
            val body =
              jsonContentOf(
                "/iam/realms/create.json",
                Map(
                  quote("{realm}") -> s"${config.realmSuffix(realm)}"
                )
              )
            cl.put[Json](s"/realms/$realm", body, identity) {
              (_, response) =>
                response.status shouldEqual StatusCodes.Created
            }
            cl.get[Json](s"/realms/$realm", Identity.ServiceAccount) {
              (_, response) =>
                response.status shouldEqual StatusCodes.OK
            }
          case StatusCodes.Forbidden | StatusCodes.OK =>
            logger.info(s"Realm $realm has already been created, we got status ${response.status}")
            cl.get[Json](s"/realms/$realm", Identity.ServiceAccount) {
              (_, response) =>
                response.status shouldEqual StatusCodes.OK
            }
          case s => fail(s"$s wasn't expected here and we got this response: $json")
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
          headers = identity match {
            case Anonymous => Nil
            case _ => tokensMap(identity) :: Nil
          },
          entity = body.fold(HttpEntity.Empty)
          (j => HttpEntity(ContentTypes.`application/json`, j.noSpaces))
        )
      ).flatMap { res =>
        Task.deferFuture {
          um(res.entity)(global, materializer)
        }.map {
          assertResponse(_, res)
        }.onErrorHandleWith { e =>
          for {
            // Deserializing to case class may fail, json should
            // be fine in almost every case
            json <- Task.deferFuture {
              implicitly[FromEntityUnmarshaller[Json]].apply(res.entity)(global, materializer)
            }.onErrorHandle {
              _ => Json.Null
            }
            _   <- Task {
              logger.error(s"Status ${res.status}", e)
              logger.error(json.spaces2)
            }
          } yield {
            fail(s"Something went wrong while processing the response for url: ${method.value} $url with identity $identity", e)
          }
        }
      }.runSyncUnsafe()
  }
}

object NewBaseSpec {

}
