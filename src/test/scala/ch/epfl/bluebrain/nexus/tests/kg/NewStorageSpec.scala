package ch.epfl.bluebrain.nexus.tests.kg

import java.io.File
import java.net.URI
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.StatusCodes
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.CirceEq
import ch.epfl.bluebrain.nexus.tests.HttpClientDsl._
import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.Optics._
import ch.epfl.bluebrain.nexus.tests.Tags.StorageTag
import ch.epfl.bluebrain.nexus.tests.config.ConfigLoader._
import ch.epfl.bluebrain.nexus.tests.config.{S3Config, StorageConfig}
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission
import ch.epfl.bluebrain.nexus.tests.iam.types.Permission.Organizations
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec, Realm}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import monix.execution.Scheduler.Implicits.global
import software.amazon.awssdk.auth.credentials.{AnonymousCredentialsProvider, AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model._

import scala.jdk.CollectionConverters._
import scala.reflect.io.Directory

class NewStorageSpec extends NewBaseSpec with CirceEq {

  val storageConfig: StorageConfig = load[StorageConfig](ConfigFactory.load(), "storage")
  val s3Config: S3Config = storageConfig.s3

  private val orgId   = genId()
  private val projId  = genId()
  private val fullId  = s"$orgId/$projId"
  private val bucket  = "nexustest"
  private val logoKey = "some/path/to/nexus-logo.png"

  private val remoteFolder = genId()

  private val testRealm   = Realm("storage" + genString())
  private val testClient = Identity.ClientCredentials(genString(), genString(), testRealm)
  private val BipBip = UserCredentials(genString(), genString(), testRealm)
  private val Coyote = UserCredentials(genString(), genString(), testRealm)

  val s3Endpoint: String = s"http://minio:9000"
  val s3BucketEndpoint: String = s"http://$bucket.minio:9000"

  val externalEndpoint: String = s"http://storage-service:8080/v1"

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
    initRealm(
      testRealm,
      Identity.ServiceAccount,
      testClient,
      BipBip :: Coyote :: Nil
    ).runSyncUnsafe()
    // Configure minio
    s3Client.createBucket(CreateBucketRequest.builder.bucket(bucket).build)
    s3Client.putObject(
      PutObjectRequest.builder.bucket(bucket).key(logoKey).build,
      Paths.get(getClass.getResource("/kg/files/nexus-logo.png").toURI)
    )
    // Create folder for remote storage
    Files.createDirectories(Paths.get(s"/tmp/storage/$remoteFolder/protected"))
    ()
  }

  override def afterAll(): Unit = {
    val objects = s3Client.listObjects(ListObjectsRequest.builder.bucket(bucket).build)
    objects.contents.asScala.foreach { obj =>
      s3Client.deleteObject(DeleteObjectRequest.builder.bucket(bucket).key(obj.key).build)
    }
    s3Client.deleteBucket(DeleteBucketRequest.builder.bucket(bucket).build)
    new Directory(new File(s"/tmp/storage/$remoteFolder")).deleteRecursively()
    super.afterAll()
  }

  "creating projects" should {

    "add necessary ACLs for user" taggedAs StorageTag in {
      aclDsl.addPermission(
        "/",
        Coyote,
        Organizations.Create
      )
    }

    "succeed if payload is correct" taggedAs StorageTag in {
      for {
        _ <- adminDsl.createOrganization(orgId, orgId, Coyote)
        _ <- adminDsl.createProject(orgId, projId, kgDsl.projectJson(name = fullId) , Coyote)
      } yield succeed
    }

  }

  "creating a storage" should {
    "succeed creating a DiskStorage" taggedAs StorageTag in {
      val payload = jsonContentOf("/kg/storages/disk.json")
      val payload2 = jsonContentOf("/kg/storages/disk-perms.json")

      for {
        _ <- cl.post[Json](s"/storages/$fullId", payload, Coyote) {
          (_, response) =>
            response.status shouldEqual StatusCodes.Created
        }
        _ <- cl.get[Json](s"/storages/$fullId/nxv:mystorage", Coyote) {
          (json, response) =>
            val expected = jsonContentOf(
              "/kg/storages/disk-response.json",
              replacements(
                Coyote,
                quote("{id}")          -> "nxv:mystorage",
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
          Permission("disk","read"), Permission("disk","write")
        )
        _ <- cl.post[Json](s"/storages/$fullId", payload2, Coyote) {
          (_, response) =>
            response.status shouldEqual StatusCodes.Created
        }
        _ <- cl.get[Json](s"/storages/$fullId/nxv:mystorage2", Coyote) {
          (json, response) =>
            val expected = jsonContentOf(
              "/kg/storages/disk-response.json",
              replacements(
                Coyote,
                quote("{id}")          -> "nxv:mystorage2",
                quote("{project}")     -> fullId,
                quote("{read}")        -> "disk/read",
                quote("{maxFileSize}") -> storageConfig.maxFileSize.toString,
                quote("{write}")       -> "disk/write"
              )
            )
            filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
            response.status shouldEqual StatusCodes.OK
        }
      } yield succeed
    }

    def serviceAccountToken = tokensMap.get(Identity.ServiceAccount).credentials.token()

    "succeed creating a RemoteDiskStorage" taggedAs StorageTag in {
      val payload = jsonContentOf(
        "/kg/storages/remote-disk.json",
        Map(
          quote("{endpoint}") -> externalEndpoint,
          quote("{cred}")     -> serviceAccountToken,
          quote("{read}")     -> "resources/read",
          quote("{write}")    -> "files/write",
          quote("{folder}")   -> remoteFolder,
          quote("{id}")       -> "myexternalstorage"
        )
      )

      val payload2 = jsonContentOf(
        "/kg/storages/remote-disk.json",
        Map(
          quote("{endpoint}") -> externalEndpoint,
          quote("{cred}")     -> serviceAccountToken,
          quote("{read}")     -> "disk/extread",
          quote("{write}")    -> "disk/extwrite",
          quote("{folder}")   -> remoteFolder,
          quote("{id}")       -> "myexternalstorage2"
        )
      )

      for {
        _ <- cl.post[Json](s"/storages/$fullId", payload, Coyote) {
          (_, response ) =>
            response.status shouldEqual StatusCodes.Created
        }
        _ <- cl.get[Json](s"/storages/$fullId/nxv:myexternalstorage", Coyote) {
          (json, response) =>
          val expected = jsonContentOf(
            "/kg/storages/remote-disk-response.json",
            replacements(
              Coyote,
              quote("{endpoint}")    -> externalEndpoint,
              quote("{folder}")      -> remoteFolder,
              quote("{id}")          -> "nxv:myexternalstorage",
              quote("{project}")     -> fullId,
              quote("{maxFileSize}") -> storageConfig.maxFileSize.toString,
              quote("{read}")        -> "resources/read",
              quote("{write}")       -> "files/write"
            )
          )
          filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
          response.status shouldEqual StatusCodes.OK
        }
        _ <- cl.get[Json](s"/storages/$fullId/nxv:myexternalstorage/source", Coyote) {
          (json, response) =>
            response.status shouldEqual StatusCodes.OK
            val expected = jsonContentOf(
              "/kg/storages/storage-source.json",
              Map(
                quote("{folder}")      -> remoteFolder,
                quote("{storageBase}") -> externalEndpoint
              )
            )
            json should equalIgnoreArrayOrder(expected)

        }
        _ <- permissionDsl.addPermissions(
          Permission("disk","extread"), Permission("disk","extwrite")
        )
        _ <- cl.post[Json](s"/storages/$fullId", payload2, Coyote) {
          (_, response ) =>
            response.status shouldEqual StatusCodes.Created
        }
        _ <- cl.get[Json](s"/storages/$fullId/nxv:myexternalstorage2", Coyote) {
          (json, response) =>
            val expected = jsonContentOf(
              "/kg/storages/remote-disk-response.json",
              replacements(
                Coyote,
                quote("{endpoint}")    -> externalEndpoint,
                quote("{folder}")      -> remoteFolder,
                quote("{id}")          -> "nxv:myexternalstorage2",
                quote("{project}")     -> fullId,
                quote("{maxFileSize}") -> storageConfig.maxFileSize.toString,
                quote("{read}")        -> "disk/extread",
                quote("{write}")       -> "disk/extwrite"
              )
            )
            filterMetadataKeys(json) should equalIgnoreArrayOrder(expected)
            response.status shouldEqual StatusCodes.OK
        }
      } yield succeed
    }

    "succeed creating an S3Storage" taggedAs StorageTag in {
      val payload = jsonContentOf(
        "/kg/storages/s3.json",
        Map(
          quote("{storageId}") -> "https://bluebrain.github.io/nexus/vocabulary/mys3storage",
          quote("{bucket}")    -> bucket,
          quote("{endpoint}")  -> s3Endpoint,
          quote("{accessKey}") -> s3Config.accessKey.get,
          quote("{secretKey}") -> s3Config.secretKey.get
        )
      )

      val payload2 = jsonContentOf(
        "/kg/storages/s3.json",
        Map(
          quote("{storageId}") -> "https://bluebrain.github.io/nexus/vocabulary/mys3storage2",
          quote("{bucket}")    -> bucket,
          quote("{endpoint}")  -> s3Endpoint,
          quote("{accessKey}") -> s3Config.accessKey.get,
          quote("{secretKey}") -> s3Config.secretKey.get
        )
      ) deepMerge Json.obj(
        "region"   -> Json.fromString("not-important"),
        "readPermission"  -> Json.fromString("s3/read"),
        "writePermission" -> Json.fromString("s3/write")
      )

      for {
        _ <- cl.post[Json](s"/storages/$fullId", payload, Coyote) {
          (_, response) =>
            response.status shouldEqual StatusCodes.Created
        }
        _ <- cl.get[Json](s"/storages/$fullId/nxv:mys3storage", Coyote) {
          (json, response) =>
            val expected = jsonContentOf(
              "/kg/storages/s3-response.json",
              replacements(
                Coyote,
                quote("{id}")          -> "nxv:mys3storage",
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
        _ <- permissionDsl.addPermissions(Permission("s3","read"), Permission("s3","write"))
        _ <- cl.post[Json](s"/storages/$fullId", payload2, Coyote) {
          (_, response) =>
            response.status shouldEqual StatusCodes.Created
        }
        _ <- cl.get[Json](s"/storages/$fullId/nxv:mys3storage2", Coyote) {
          (json, response) =>
            val expected = jsonContentOf(
              "/kg/storages/s3-response.json",
              replacements(
                Coyote,
                quote("{id}")          -> "nxv:mys3storage2",
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

    "fail creating a RemoteDiskStorage without folder" taggedAs StorageTag in {
      val payload = jsonContentOf(
        "/kg/storages/remote-disk.json",
        Map(
          quote("{endpoint}") -> externalEndpoint,
          quote("{cred}")     -> serviceAccountToken,
          quote("{read}")     -> "resources/read",
          quote("{write}")    -> "files/write",
          quote("{folder}")   -> "nexustest",
          quote("{id}")       -> "myexternalstorage"
        )
      )

      cl.post[Json](s"/storages/$fullId", filterKey("folder")(payload), Coyote) {
        (_, response) => response.status shouldEqual StatusCodes.BadRequest
      }
    }

    "wait for storages to be indexed" taggedAs StorageTag in {
      eventually {
        cl.get[Json](s"/storages/$fullId", Coyote) {
          (json, response) =>
            response.status shouldEqual StatusCodes.OK
            _total.getOption(json).value shouldEqual 7
        }
      }
    }
  }

  "uploading an attachment against the S3 storage" should {
    "link an existing file" taggedAs StorageTag in {
      val payload = Json.obj(
        "filename"  -> Json.fromString("logo.png"),
        "path"      -> Json.fromString(logoKey),
        "mediaType" -> Json.fromString("image/png")
      )

      cl.put[Json](s"/files/$fullId/logo.png?storage=nxv:mys3storage", payload, Coyote) {
        (json, response) =>
          println(json)
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

    "fail to link a nonexistent file" taggedAs StorageTag in {
      val payload = Json.obj(
        "filename"  -> Json.fromString("logo.png"),
        "path"      -> Json.fromString("non/existent.png"),
        "mediaType" -> Json.fromString("image/png")
      )

      cl.put[Json](s"/files/$fullId/logo.png?storage=nxv:mys3storage", payload, Coyote) {
        (json, response) =>
          println(json)
          response.status shouldEqual StatusCodes.BadGateway
          json shouldEqual
            json shouldEqual jsonContentOf(
            "/kg/files/linking-notfound.json",
            Map(quote("{endpointBucket}") -> s3BucketEndpoint)
          )
      }
    }
  }

  "uploading an attachment against the ExternalDisk storage" should {


  }
}
