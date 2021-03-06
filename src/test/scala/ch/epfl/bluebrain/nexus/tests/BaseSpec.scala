package ch.epfl.bluebrain.nexus.tests

import java.util.regex.Pattern.quote

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import cats.implicits._
import ch.epfl.bluebrain.nexus.commons.http.HttpClient
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity._
import ch.epfl.bluebrain.nexus.tests.admin.AdminDsl
import ch.epfl.bluebrain.nexus.tests.config.ConfigLoader._
import ch.epfl.bluebrain.nexus.tests.config.{PrefixesConfig, TestsConfig}
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission
import ch.epfl.bluebrain.nexus.tests.iam.{AclDsl, PermissionDsl}
import ch.epfl.bluebrain.nexus.tests.kg.KgDsl
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpecLike
import org.scalatest.{Assertion, BeforeAndAfterAll, OptionValues}

import scala.concurrent.Future
import scala.concurrent.duration._

trait BaseSpec
    extends AsyncWordSpecLike
    with BeforeAndAfterAll
    with Resources
    with Randomness
    with ScalatestRouteTest
    with Eventually
    with OptionValues
    with ScalaFutures
    with Matchers {

  private val logger = Logger[this.type]

  implicit val config: TestsConfig = load[TestsConfig](ConfigFactory.load(), "tests")

  val prefixesConfig: PrefixesConfig = load[PrefixesConfig](ConfigFactory.load(), "prefixes")

  private[tests] implicit val cl: UntypedHttpClient[Task] = HttpClient.untyped[Task]

  val aclDsl        = new AclDsl()
  val permissionDsl = new PermissionDsl()
  val adminDsl      = new AdminDsl(prefixesConfig, config)
  val kgDsl         = new KgDsl(config)

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(config.patience, 300.millis)

  implicit def taskToFutureAssertion(task: Task[Assertion]): Future[Assertion] =
    task.runToFuture

  implicit def taskListToFutureAssertion(task: Task[List[Assertion]]): Future[Assertion] =
    task.runToFuture.map(_ => succeed)(global)

  implicit def futureListToFutureAssertion(future: Future[List[Assertion]]): Future[Assertion] =
    future.map(_ => succeed)(global)

  def eventually(t: Task[Assertion]): Assertion =
    eventually {
      t.runSyncUnsafe()
    }

  def runTask[A](t: Task[A]): Assertion =
    t.map { _ =>
        succeed
      }
      .runSyncUnsafe()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val setup = for {
      _ <- Elasticsearch.createTemplate
      _ <- initRealm(
        Realm.internal,
        Identity.Anonymous,
        Identity.ServiceAccount,
        Identity.users
      )
      _ <- aclDsl.addPermissions(
        "/",
        Identity.ServiceAccount,
        Permission.minimalPermissions
      )
    } yield ()
    setup.runSyncUnsafe()
  }

  private def toAuthorizationHeader(token: String) =
    Authorization(
      HttpCredentials.createOAuth2BearerToken(token)
    )

  private[tests] def authenticateUser(user: UserCredentials, client: ClientCredentials): Task[Unit] = {
    Keycloak.userToken(user, client).map { token =>
      logger.info(s"Token for user ${user.name} is: $token")
      tokensMap.put(user, toAuthorizationHeader(token))
      ()
    }
  }

  private[tests] def authenticateClient(client: ClientCredentials): Task[Unit] = {
    Keycloak.serviceAccountToken(client).map { token =>
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
  def initRealm(
      realm: Realm,
      identity: Identity,
      client: ClientCredentials,
      users: List[UserCredentials]
  ): Task[Unit] = {
    def createRealmInDelta: Task[Assertion] = cl.get[Json](s"/realms/${realm.name}", identity) { (json, response) =>
      runTask {
        response.status match {
          case StatusCodes.NotFound =>
            logger.info(s"Realm ${realm.name} is absent, we create it")
            val body =
              jsonContentOf(
                "/iam/realms/create.json",
                Map(
                  quote("{realm}") -> s"${config.realmSuffix(realm)}"
                )
              )
            for {
              _ <- cl.put[Json](s"/realms/${realm.name}", body, identity) { (_, response) =>
                response.status shouldEqual StatusCodes.Created
              }
              _ <- cl.get[Json](s"/realms/${realm.name}", Identity.ServiceAccount) { (_, response) =>
                response.status shouldEqual StatusCodes.OK
              }
            } yield ()
          case StatusCodes.Forbidden | StatusCodes.OK =>
            logger.info(s"Realm ${realm.name} has already been created, we got status ${response.status}")
            cl.get[Json](s"/realms/${realm.name}", Identity.ServiceAccount) { (_, response) =>
              response.status shouldEqual StatusCodes.OK
            }
          case s =>
            Task(
              fail(s"$s wasn't expected here and we got this response: $json")
            )
        }
      }
    }

    for {
      // Create the realm in Keycloak
      _ <- Keycloak.importRealm(realm, client, users)
      // Get the tokens and cache them in the map
      _ <- users.traverse { user =>
        authenticateUser(user, client)
      }
      _ <- authenticateClient(client)
      // Creating the realm in delta
      _ <- Task { logger.info(s"Creating realm ${realm.name} in the delta instance") }
      _ <- createRealmInDelta
    } yield ()
  }

  private[tests] def dispositionType(response: HttpResponse): ContentDispositionType =
    response.header[`Content-Disposition`].value.dispositionType

  private[tests] def attachmentName(response: HttpResponse): String =
    response
      .header[`Content-Disposition`]
      .value
      .params
      .get("filename")
      .value

  private[tests] def contentType(response: HttpResponse): ContentType =
    response.header[`Content-Type`].value.contentType

  private[tests] def httpEncodings(response: HttpResponse): Seq[HttpEncoding] =
    response.header[`Content-Encoding`].value.encodings

  private[tests] def decodeGzip(input: ByteString): String =
    Gzip.decode(input).map(_.utf8String)(global).futureValue

  private[tests] def replacements(authenticated: Authenticated, otherReplacements: (String, String)*) =
    Map(
      quote("{deltaUri}") -> config.deltaUri.toString(),
      quote("{realm}")    -> authenticated.realm.name,
      quote("{user}")     -> authenticated.name
    ) ++ otherReplacements.toMap

  private[tests] def genId(length: Int = 15): String =
    genString(length = length, Vector.range('a', 'z') ++ Vector.range('0', '9'))

}
