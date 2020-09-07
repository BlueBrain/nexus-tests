package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.Randomness
import ch.epfl.bluebrain.nexus.rdf.syntax._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Tags.{IamTag, RealmsTag}
import ch.epfl.bluebrain.nexus.tests.{Identity, Keycloak, NewBaseSpec}
import io.circe.Json

class RealmsSpec extends NewBaseSpec with Randomness {

  val removedKeys = List("_createdAt", "_createdBy", "_updatedAt", "_updatedBy")

  private val testRealm   = "realm" + genString()
  private val testRealmUri = config.realmSuffix(testRealm)

  private val testClient = Identity.ClientCredentials(genString(), genString())

  override def beforeAll(): Unit = {
    super.beforeAll()

    val users = List(
      UserCredentials(genString(), genString()),
      UserCredentials(genString(), genString())
    )

    Keycloak.importRealm(testRealm, testClient, users) shouldEqual StatusCodes.Created
    authenticateClient(testRealm, testClient)
    ()
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
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> testRealm,
              quote("{rev}")        -> "1",
              quote("{deprecated}") -> "false"
            )
          )
      }
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
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> testRealm,
              quote("{rev}")        -> s"${rev + 1}",
              quote("{deprecated}") -> "false"
            )
          )
      }
    }

    "fetch realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$testRealm", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/fetch-response.json",
            Map(
              quote("{realm}")     -> testRealmUri,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{rev}")       -> s"${rev + 1}",
              quote("{label}")     -> testRealm
            )
          )
      }
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
          json.removeKeys(removedKeys: _*) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}") -> testRealmUri,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{label}") -> testRealm,
              quote("{rev}") -> s"${rev + 2}",
              quote("{deprecated}") -> "false"
            )
          )
      }
    }

    "fetch updated realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$testRealm", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/fetch-updated-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{rev}")        -> s"${rev + 2}",
              quote("{label}")      -> testRealm
            )
          )
      }
    }

    "deprecate realm" taggedAs (IamTag, RealmsTag) in {
      cl.delete[Json](s"/realms/$testRealm?rev=${rev + 2}", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> testRealmUri,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> testRealm,
              quote("{rev}")        -> s"${rev + 3}",
              quote("{deprecated}") -> "true"
            )
          )
      }
    }

    "fetch deprecated realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$testRealm", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/fetch-deprecated-response.json",
            Map(
              quote("{realm}")     -> testRealmUri,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{rev}")       -> s"${rev + 3}",
              quote("{label}")     -> testRealm
            )
          )
      }
    }
  }
}
