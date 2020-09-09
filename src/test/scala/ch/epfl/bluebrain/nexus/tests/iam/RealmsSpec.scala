package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.{IamTag, RealmsTag}
import ch.epfl.bluebrain.nexus.tests.{Identity, Keycloak, NewBaseSpec}
import ch.epfl.bluebrain.nexus.tests.DeltaHttpClient._
import monix.execution.Scheduler.Implicits.global
import io.circe.Json

class RealmsSpec extends NewBaseSpec {

  private val testRealm   = "realm" + genString()
  private val testRealmUri = config.realmSuffix(testRealm)

  private val testClient = Identity.ClientCredentials(genString(), genString())

  override def beforeAll(): Unit = {
    super.beforeAll()

    val users = List(
      UserCredentials(genString(), genString()),
      UserCredentials(genString(), genString())
    )

    val setup = for {
        _ <- Keycloak.importRealm(testRealm, testClient, users).map { _ shouldEqual StatusCodes.Created }
        _ <- authenticateClient(testRealm, testClient)
      } yield ()

    setup.runSyncUnsafe()
  }

  "manage realms" should {
    val rev = 1L

    "create realm" taggedAs (IamTag, RealmsTag) in {
      val body = jsonContentOf(
        "/iam/realms/create.json",
        Map(
          quote("{realm}") -> testRealmUri
        )
      )

      cl.put[Json](s"/realms/$testRealm", body, Identity.ServiceAccount) {
        (json, _) =>
          filterRealmKeys(json) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> testRealm,
              quote("{rev}")        -> "1",
              quote("{deprecated}") -> "false"
            )
          )
      }.runSyncUnsafe()
    }

    "recreate realm" taggedAs (IamTag, RealmsTag) in {
      val body = jsonContentOf(
        "/iam/realms/create.json",
        Map(
          quote("{realm}") -> testRealmUri
        )
      )

      cl.put[Json](s"/realms/$testRealm?rev=$rev", body, Identity.ServiceAccount) {
        (json, _) =>
          filterRealmKeys(json) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> testRealm,
              quote("{rev}")        -> s"${rev + 1}",
              quote("{deprecated}") -> "false"
            )
          )
      }.runSyncUnsafe()
    }

    "fetch realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$testRealm", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          filterRealmKeys(json) shouldEqual jsonContentOf(
            "/iam/realms/fetch-response.json",
            Map(
              quote("{realm}")     -> testRealmUri,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{rev}")       -> s"${rev + 1}",
              quote("{label}")     -> testRealm
            )
          )
      }.runSyncUnsafe()
    }

    "update realm" taggedAs (IamTag, RealmsTag) in {
      val body =
        jsonContentOf(
          "/iam/realms/update.json",
          Map(
            quote("{realm}") -> testRealmUri
          )
        )

      cl.put[Json](s"/realms/$testRealm?rev=${rev + 1}", body, Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          filterRealmKeys(json) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}") -> testRealmUri,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{label}") -> testRealm,
              quote("{rev}") -> s"${rev + 2}",
              quote("{deprecated}") -> "false"
            )
          )
      }.runSyncUnsafe()
    }

    "fetch updated realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$testRealm", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          filterRealmKeys(json) shouldEqual jsonContentOf(
            "/iam/realms/fetch-updated-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{rev}")        -> s"${rev + 2}",
              quote("{label}")      -> testRealm
            )
          )
      }.runSyncUnsafe()
    }

    "deprecate realm" taggedAs (IamTag, RealmsTag) in {
      cl.delete[Json](s"/realms/$testRealm?rev=${rev + 2}", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          filterRealmKeys(json) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> testRealm,
              quote("{rev}")        -> s"${rev + 3}",
              quote("{deprecated}") -> "true"
            )
          )
      }.runSyncUnsafe()
    }

    "fetch deprecated realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$testRealm", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          filterRealmKeys(json) shouldEqual jsonContentOf(
            "/iam/realms/fetch-deprecated-response.json",
            Map(
              quote("{realm}")     -> testRealmUri,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{rev}")       -> s"${rev + 3}",
              quote("{label}")     -> testRealm
            )
          )
      }.runSyncUnsafe()
    }
  }
}
