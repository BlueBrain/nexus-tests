package ch.epfl.bluebrain.nexus.tests.kg
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Optics.filterMetadataKeys
import ch.epfl.bluebrain.nexus.tests.Tags.StorageTag
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission
import io.circe.Json
import monix.bio.Task
import org.scalatest.Assertion

class DiskStorageSpec extends StorageSpec {

  override def storageType: String = "disk"

  override def storageName: String = "mystorage"

  override def locationPrefix: Option[String] = None

  override def createStorages: Task[Assertion] = {
    val payload  = jsonContentOf("/kg/storages/disk.json")
    val payload2 = jsonContentOf("/kg/storages/disk-perms.json")

    for {
      _ <- cl.post[Json](s"/storages/$fullId", payload, Coyote) { (_, response) =>
        response.status shouldEqual StatusCodes.Created
      }
      _ <- cl.get[Json](s"/storages/$fullId/nxv:$storageName", Coyote) { (json, response) =>
        val expected = jsonContentOf(
          "/kg/storages/disk-response.json",
          replacements(
            Coyote,
            quote("{id}")          -> s"nxv:$storageName",
            quote("{project}")     -> fullId,
            quote("{read}")        -> "resources/read",
            quote("{maxFileSize}") -> storageConfig.maxFileSize.toString,
            quote("{write}")       -> "files/write"
          )
        )
        filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
        response.status shouldEqual StatusCodes.OK
      }
      _ <- permissionDsl.addPermissions(
        Permission(storageType, "read"),
        Permission(storageType, "write")
      )
      _ <- cl.post[Json](s"/storages/$fullId", payload2, Coyote) { (_, response) =>
        response.status shouldEqual StatusCodes.Created
      }
      _ <- cl.get[Json](s"/storages/$fullId/nxv:${storageName}2", Coyote) { (json, response) =>
        val expected = jsonContentOf(
          "/kg/storages/disk-response.json",
          replacements(
            Coyote,
            quote("{id}")          -> s"nxv:${storageName}2",
            quote("{project}")     -> fullId,
            quote("{read}")        -> s"$storageType/read",
            quote("{maxFileSize}") -> storageConfig.maxFileSize.toString,
            quote("{write}")       -> s"$storageType/write"
          )
        )
        filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
        response.status shouldEqual StatusCodes.OK
      }
    } yield succeed
  }

  "creating a disk storage" should {
    "fail creating a DiskStorage on a wrong volume" taggedAs StorageTag in {
      val volume = "/" + genString()
      val payload = jsonContentOf("/kg/storages/disk.json") deepMerge
        Json.obj("volume" -> Json.fromString(volume))

      cl.post[Json](s"/storages/$fullId", payload, Coyote) { (json, response) =>
        json shouldEqual jsonContentOf("/kg/storages/error.json", Map(quote("{volume}") -> volume))
        response.status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  s"Linking against the default storage" should {
    "reject linking operations" taggedAs StorageTag in {
      val payload = Json.obj(
        "filename"  -> Json.fromString("logo.png"),
        "path"      -> Json.fromString("does/not/matter"),
        "mediaType" -> Json.fromString("image/png")
      )

      cl.put[Json](s"/files/$fullId/linking.png", payload, Coyote) { (json, response) =>
        response.status shouldEqual StatusCodes.BadRequest
        json shouldEqual jsonContentOf("/kg/files/linking-notsupported.json")
      }
    }
  }
}
