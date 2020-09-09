package ch.epfl.bluebrain.nexus.tests

import java.util.regex.Pattern.quote

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.HttpClient
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.DeltaHttpClient._
import ch.epfl.bluebrain.nexus.tests.Identity._
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.config.ConfigLoader._
import ch.epfl.bluebrain.nexus.tests.config.TestsConfig
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclListing, Permission}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{Assertion, BeforeAndAfterAll}

import scala.concurrent.duration._

trait NewBaseSpec extends AnyWordSpecLike
  with BeforeAndAfterAll
  with Resources
  with Randomness
  with ScalatestRouteTest
  with ScalaFutures
  with Matchers {

  private val logger = Logger[this.type]

  implicit val config: TestsConfig = load[TestsConfig](ConfigFactory.load(), "tests")

  private[tests] val internalRealm = "internal"

  private[tests] implicit val cl: UntypedHttpClient[Task] = HttpClient.untyped[Task]

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(config.patience, 300.millis)

  def runTask[A](t: Task[A]): Assertion =
    t.map { _ => succeed }
      .runSyncUnsafe()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val setup = for {
      _ <- initRealm(
        internalRealm,
        Identity.Anonymous,
        Identity.ServiceAccount,
        Identity.users
      )
      _ <- addPermissions(
        "/",
        Identity.ServiceAccount,
        internalRealm,
        Permission.minimalPermissions
      )
    } yield ()
    setup.runSyncUnsafe()
  }

  private def toAuthorizationHeader(token: String) =
    Authorization(
      HttpCredentials.createOAuth2BearerToken(token)
    )

  private[tests] def authenticateUser(realm: String, user: UserCredentials, client: ClientCredentials): Task[Unit] = {
    Keycloak.userToken(realm, user, client).map { token =>
      tokensMap.put(user, toAuthorizationHeader(token))
      ()
    }
  }

  private[tests] def authenticateClient(realm: String, client: ClientCredentials): Task[Unit] = {
    Keycloak.serviceAccountToken(realm, client).map { token =>
        tokensMap.put(client, toAuthorizationHeader(token))
      ()
    }
  }

  /**
   * Init a new realm both in Keycloak and in Delta and
   * Retrieve tokens for the new clients and users
   *
   * @param realm the name of the realm to create
   * @param identity the identity responsible of creating the realm in delta
   * @param client the service account to create for the realm
   * @param users the users to create in the realm
   * @return
   */
  def initRealm(realm: String,
                identity: Identity,
                client: ClientCredentials,
                users: List[UserCredentials]): Task[Unit] = {
    def createRealmInDelta: Task[Assertion] = cl.get[Json](s"/realms/$realm", identity) {
      (json, response) =>
        runTask {
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
              for {
                _ <- cl.put[Json](s"/realms/$realm", body, identity) {
                  (_, response) => response.status shouldEqual StatusCodes.Created
                }
                _ <- cl.get[Json](s"/realms/$realm", Identity.ServiceAccount) {
                  (_, response) =>
                    response.status shouldEqual StatusCodes.OK
                }
              } yield ()
            case StatusCodes.Forbidden | StatusCodes.OK =>
              logger.info(s"Realm $realm has already been created, we got status ${response.status}")
              cl.get[Json](s"/realms/$realm", Identity.ServiceAccount) {
                (_, response) =>
                  response.status shouldEqual StatusCodes.OK
              }
            case s =>
              Task (
                fail(s"$s wasn't expected here and we got this response: $json")
              )
          }
        }
    }

    for {
      // Create the realm in Keycloak
      _ <- Keycloak.importRealm(realm, client, users)
      // Get the tokens and cache them in the map
      _ <- users.traverse { user => authenticateUser(realm, user, client) }
      _ <- authenticateClient(realm, client)
      // Creating the realm in delta
      _ <- Task { logger.info(s"Creating realm $realm in the delta instance") }
      _ <- createRealmInDelta
    } yield ()
  }

  def addPermission(path: String,
                     target: Authenticated,
                     realm: String,
                     permission: Permission): Task[Assertion] =
    addPermissions(path, target, realm, Set(permission))

  def addPermissions(path: String,
                     target: Authenticated,
                     realm: String,
                     permissions: Set[Permission]): Task[Assertion] = {
    path should not startWith("/acls")

    val permissionsMap = Map(
      quote("{realm}") -> realm,
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
        case s => fail(s"We were not expecting $s when setting acls on $path for realm $realm for ${target.name}")
      }

    cl.get[AclListing](s"/acls$path", Identity.ServiceAccount) { (acls, response) =>
      runTask {
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
      }
    }
  }

  private[tests] def genId(length: Int = 15): String =
    genString(length = length, Vector.range('a', 'z') ++ Vector.range('0', '9'))

}

