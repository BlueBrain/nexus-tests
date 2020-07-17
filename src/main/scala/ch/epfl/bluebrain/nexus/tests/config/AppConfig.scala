package ch.epfl.bluebrain.nexus.tests.config

import java.net.URI

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Host
import ch.epfl.bluebrain.nexus.tests.config.AppConfig._

import scala.concurrent.duration.FiniteDuration

/**
  * Case class which aggregates the configuration parameters
  *
  * @param http            http settings
  * @param kg              kg connection settings
  * @param admin           admin connection settings
  * @param iam             IAM connection settings
  * @param prefixes        the collection of prefixes used throughout the service
  * @param storage         storage settings
  */
final case class AppConfig(
    http: HttpConfig,
    kg: KgConfig,
    admin: AdminConfig,
    iam: IamConfig,
    prefixes: PrefixesConfig,
    storage: StorageConfig
)

object AppConfig {

  final case class HttpConfig(patienceConfig: FiniteDuration)

  final case class KgConfig(uri: Uri, version: Uri, status: Uri)

  final case class AdminConfig(uri: Uri, attachmentSize: Long)

  final case class IamConfig(
      uri: Uri,
      testRealm: String,
      serviceAccountToken: String,
      testUserToken: String,
      testUserSub: String,
      coreContext: Uri
  )

  final case class StorageConfig(s3: S3Config, external: ExternalStorageConfig, maxFileSize: Long)

  final case class ExternalStorageConfig(endpoint: Uri, credentials: String)

  final case class PrefixesConfig(
      coreContext: Uri,
      standardsContext: Uri,
      linksContext: Uri,
      searchContext: Uri,
      distributionContext: Uri,
      errorContext: Uri
  )

  final case class S3Config(endpoint: Uri, accessKey: Option[String], secretKey: Option[String]) {
    val endpointURI: URI = new URI(endpoint.toString)
    def endpointWithSubdomain(bucket: String): Uri =
      endpoint.copy(authority = endpoint.authority.copy(host = Host(s"$bucket.${endpoint.authority.host.toString()}")))

  }
}
