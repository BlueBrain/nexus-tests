package ch.epfl.bluebrain.nexus.tests.kg

import java.net.URI
import java.nio.file.Paths
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Optics.filterMetadataKeys
import ch.epfl.bluebrain.nexus.tests.Tags.StorageTag
import ch.epfl.bluebrain.nexus.tests.config.S3Config
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission
import io.circe.Json
import monix.bio.Task
import org.scalatest.Assertion
import software.amazon.awssdk.auth.credentials.{
  AnonymousCredentialsProvider,
  AwsBasicCredentials,
  StaticCredentialsProvider
}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model._

import scala.jdk.CollectionConverters._

class S3StorageSpec extends StorageSpec {

  override def storageType: String = "s3"

  override def storageName: String = "mys3storage"

  override def locationPrefix: Option[String] = Some(s3BucketEndpoint)

  val s3Config: S3Config = storageConfig.s3

  private val bucket  = "nexustest"
  private val logoKey = "some/path/to/nexus-logo.png"

  val s3Endpoint: String       = s"http://delta.bbp:9000"
  val s3BucketEndpoint: String = s"http://$bucket.delta.bbp:9000"

  private val credentialsProvider = (s3Config.accessKey, s3Config.secretKey) match {
    case (Some(ak), Some(sk)) => StaticCredentialsProvider.create(AwsBasicCredentials.create(ak, sk))
    case _                    => AnonymousCredentialsProvider.create()
  }

  private val s3Client = S3Client.builder
    .endpointOverride(new URI(s"http://${System.getProperty("minio:9000")}"))
    .credentialsProvider(credentialsProvider)
    .region(Region.US_EAST_1)
    .build

  override def beforeAll(): Unit = {
    super.beforeAll()
    // Configure minio
    s3Client.createBucket(CreateBucketRequest.builder.bucket(bucket).build)
    s3Client.putObject(
      PutObjectRequest.builder.bucket(bucket).key(logoKey).build,
      Paths.get(getClass.getResource("/kg/files/nexus-logo.png").toURI)
    )
    ()
  }

  override def afterAll(): Unit = {
    val objects = s3Client.listObjects(ListObjectsRequest.builder.bucket(bucket).build)
    objects.contents.asScala.foreach { obj =>
      s3Client.deleteObject(DeleteObjectRequest.builder.bucket(bucket).key(obj.key).build)
    }
    s3Client.deleteBucket(DeleteBucketRequest.builder.bucket(bucket).build)
    super.afterAll()
  }

  override def createStorages: Task[Assertion] = {
    val payload = jsonContentOf(
      "/kg/storages/s3.json",
      Map(
        quote("{storageId}") -> s"https://bluebrain.github.io/nexus/vocabulary/$storageName",
        quote("{bucket}")    -> bucket,
        quote("{endpoint}")  -> s3Endpoint,
        quote("{accessKey}") -> s3Config.accessKey.get,
        quote("{secretKey}") -> s3Config.secretKey.get
      )
    )

    val payload2 = jsonContentOf(
      "/kg/storages/s3.json",
      Map(
        quote("{storageId}") -> s"https://bluebrain.github.io/nexus/vocabulary/${storageName}2",
        quote("{bucket}")    -> bucket,
        quote("{endpoint}")  -> s3Endpoint,
        quote("{accessKey}") -> s3Config.accessKey.get,
        quote("{secretKey}") -> s3Config.secretKey.get
      )
    ) deepMerge Json.obj(
      "region"          -> Json.fromString("not-important"),
      "readPermission"  -> Json.fromString(s"$storageType/read"),
      "writePermission" -> Json.fromString(s"$storageType/write")
    )

    for {
      _ <- cl.post[Json](s"/storages/$fullId", payload, Coyote) { (_, response) =>
        response.status shouldEqual StatusCodes.Created
      }
      _ <- cl.get[Json](s"/storages/$fullId/nxv:$storageName", Coyote) { (json, response) =>
        val expected = jsonContentOf(
          "/kg/storages/s3-response.json",
          replacements(
            Coyote,
            quote("{id}")          -> s"nxv:$storageName",
            quote("{project}")     -> fullId,
            quote("{bucket}")      -> bucket,
            quote("{maxFileSize}") -> storageConfig.maxFileSize.toString,
            quote("{endpoint}")    -> s3Endpoint,
            quote("{read}")        -> "resources/read",
            quote("{write}")       -> "files/write"
          )
        )
        filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
        response.status shouldEqual StatusCodes.OK
      }
      _ <- permissionDsl.addPermissions(Permission(storageType, "read"), Permission(storageType, "write"))
      _ <- cl.post[Json](s"/storages/$fullId", payload2, Coyote) { (_, response) =>
        response.status shouldEqual StatusCodes.Created
      }
      _ <- cl.get[Json](s"/storages/$fullId/nxv:${storageName}2", Coyote) { (json, response) =>
        val expected = jsonContentOf(
          "/kg/storages/s3-response.json",
          replacements(
            Coyote,
            quote("{id}")          -> s"nxv:${storageName}2",
            quote("{project}")     -> fullId,
            quote("{bucket}")      -> bucket,
            quote("{maxFileSize}") -> storageConfig.maxFileSize.toString,
            quote("{endpoint}")    -> s3Endpoint,
            quote("{read}")        -> "s3/read",
            quote("{write}")       -> "s3/write"
          )
        ).deepMerge(Json.obj("region" -> Json.fromString("not-important")))
        filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
        response.status shouldEqual StatusCodes.OK
      }
    } yield succeed
  }

  "creating a s3 storage" should {
    "fail creating an S3Storage with an invalid bucket" taggedAs StorageTag in {
      val payload = jsonContentOf(
        "/kg/storages/s3.json",
        Map(
          quote("{storageId}") -> s"https://bluebrain.github.io/nexus/vocabulary/$storageName",
          quote("{bucket}")    -> "foobar",
          quote("{endpoint}")  -> s3Endpoint,
          quote("{accessKey}") -> s3Config.accessKey.get,
          quote("{secretKey}") -> s3Config.secretKey.get
        )
      )

      cl.post[Json](s"/storages/$fullId", payload, Coyote) { (json, response) =>
        json shouldEqual jsonContentOf("/kg/storages/s3-error.json")
        response.status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  s"Linking in S3" should {
    "link an existing file" taggedAs StorageTag in {
      val payload = Json.obj(
        "filename"  -> Json.fromString("logo.png"),
        "path"      -> Json.fromString(logoKey),
        "mediaType" -> Json.fromString("image/png")
      )

      cl.put[Json](s"/files/$fullId/logo.png?storage=nxv:${storageName}2", payload, Coyote) { (json, response) =>
        response.status shouldEqual StatusCodes.Created
        filterMetadataKeys(json) shouldEqual
          jsonContentOf(
            "/kg/files/linking-metadata.json",
            replacements(
              Coyote,
              quote("{projId}")         -> fullId,
              quote("{endpoint}")       -> s3Endpoint,
              quote("{endpointBucket}") -> s3BucketEndpoint,
              quote("{key}")            -> logoKey
            )
          )
      }
    }
  }

  "fail to link a nonexistent file" taggedAs StorageTag in {
    val payload = Json.obj(
      "filename"  -> Json.fromString("logo.png"),
      "path"      -> Json.fromString("non/existent.png"),
      "mediaType" -> Json.fromString("image/png")
    )

    cl.put[Json](s"/files/$fullId/nonexistent.png?storage=nxv:${storageName}2", payload, Coyote) { (json, response) =>
      response.status shouldEqual StatusCodes.BadGateway
      json shouldEqual jsonContentOf(
        "/kg/files/linking-notfound.json",
        Map(quote("{endpointBucket}") -> s3BucketEndpoint)
      )
    }
  }
}
