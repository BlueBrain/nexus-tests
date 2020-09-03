package ch.epfl.bluebrain.nexus.tests.iam

import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.rdf.syntax._
import ch.epfl.bluebrain.nexus.tests.Tags.{IamTag, RealmsTag}
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import io.circe.Json
import io.circe.optics.JsonPath._

class RealmsSpec extends NewBaseSpec {

  private val revision = root._rev.long

  val removedKeys = List("_createdAt", "_createdBy", "_updatedAt", "_updatedBy")

  "manage realms" should {
    var rev = 1L

    "fetch realm revision" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$realmLabel", Identity.ServiceAccount) {
        (json, response) =>
          rev = revision.getOption(json).getOrElse(rev)
          response.status shouldEqual StatusCodes.OK
      }
    }

    "re-create realm" taggedAs (IamTag, RealmsTag) in {
      val body = jsonContentOf(
          "/iam/realms/create.json",
          Map(
            quote("{realm}") -> config.realm
          )
        )

      cl.put[Json](s"/realms/$realmLabel?rev=$rev", body, Identity.ServiceAccount) {
        (json, _) =>
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> config.realm,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> realmLabel,
              quote("{rev}")        -> s"${rev + 1}",
              quote("{deprecated}") -> "false"
            )
          )
      }
    }

    "fetch realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$realmLabel", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/fetch-response.json",
            Map(
              quote("{realm}")     -> config.realm,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{rev}")       -> s"${rev + 1}",
              quote("{label}")     -> realmLabel
            )
          )
      }
    }

    "update realm" taggedAs (IamTag, RealmsTag) in {
      val body =
        jsonContentOf(
          "/iam/realms/update.json",
          Map(
            quote("{realm}") -> config.realm
          )
        )

      cl.put[Json](s"/realms/$realmLabel?rev=${rev + 1}", body, Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys: _*) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}") -> config.realm,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{label}") -> realmLabel,
              quote("{rev}") -> s"${rev + 2}",
              quote("{deprecated}") -> "false"
            )
          )
      }
    }

    "fetch updated realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$realmLabel", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/fetch-updated-response.json",
            Map(
              quote("{realm}")      -> config.realm,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{rev}")        -> s"${rev + 2}",
              quote("{label}")      -> realmLabel
            )
          )
      }
    }

    "deprecate realm" taggedAs (IamTag, RealmsTag) in {
      cl.delete[Json](s"/realms/$realmLabel?rev=${rev + 2}", Identity.ServiceAccount) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/ref-response.json",
            Map(
              quote("{realm}")      -> config.realm,
              quote("{deltaBase}")  -> config.deltaUri.toString(),
              quote("{label}")      -> realmLabel,
              quote("{rev}")        -> s"${rev + 3}",
              quote("{deprecated}") -> "true"
            )
          )
      }
    }

    "fetch deprecated realm" taggedAs (IamTag, RealmsTag) in {
      cl.get[Json](s"/realms/$realmLabel", Identity.Anonymous) {
        (json, result) =>
          result.status shouldEqual StatusCodes.OK
          json.removeKeys(removedKeys:_*) shouldEqual jsonContentOf(
            "/iam/realms/fetch-deprecated-response.json",
            Map(
              quote("{realm}")     -> config.realm,
              quote("{deltaBase}") -> config.deltaUri.toString(),
              quote("{rev}")       -> s"${rev + 3}",
              quote("{label}")     -> realmLabel
            )
          )
      }
    }
  }
}
