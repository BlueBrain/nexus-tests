package ch.epfl.bluebrain.nexus.tests.config

import java.net.URI

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.tests.config.AppConfig._

import scala.concurrent.duration.FiniteDuration

/**
  * Case class which aggregates the configuration parameters
  *
  * @param http     http settings
  * @param kg       kg connection settings
  * @param admin    admin connection settings
  * @param iam      IAM connection settings
  * @param prefixes the collection of prefixes used throughout the service
  * @param external external storage connection settings
  * @param s3       the S3 storage backend settings
  */
final case class AppConfig(http: HttpConfig,
                           kg: KgConfig,
                           admin: AdminConfig,
                           iam: IamConfig,
                           prefixes: PrefixesConfig,
                           external: ExternalStorageConfig,
                           s3: S3Config)

object AppConfig {

  final case class HttpConfig(patienceConfig: FiniteDuration)

  final case class KgConfig(uri: Uri)

  final case class AdminConfig(uri: Uri, attachmentSize: Long)

  final case class IamConfig(uri: Uri,
                             testRealm: String,
                             groupToken: String,
                             userToken: String,
                             userSub: String,
                             coreContext: Uri)

  final case class ExternalStorageConfig(endpoint: Uri, credentials: String)

  final case class PrefixesConfig(coreContext: Uri,
                                  standardsContext: Uri,
                                  linksContext: Uri,
                                  searchContext: Uri,
                                  distributionContext: Uri,
                                  errorContext: Uri)

  final case class S3Config(endpoint: Uri, accessKey: Option[String], secretKey: Option[String]) {
    val endpointURI: URI = new URI(endpoint.toString)
  }
}
