package ch.epfl.bluebrain.nexus.tests.config

import java.net.URI

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Host
import com.typesafe.config.Config
import pureconfig.ConvertHelpers.catchReadError
import pureconfig.{ConfigConvert, ConfigSource}

import scala.concurrent.duration.FiniteDuration

final case class TestsConfig(deltaUri: Uri,
                             realmUri: Uri,
                             patience: FiniteDuration) {

  def realmSuffix(realm: String) = s"$realmUri/$realm"
}

final case class PrefixesConfig(iamCoreContext: Uri,
                                coreContext: Uri,
                                standardsContext: Uri,
                                linksContext: Uri,
                                searchContext: Uri,
                                distributionContext: Uri,
                                errorContext: Uri)

final case class StorageConfig(s3: S3Config, external: ExternalStorageConfig, maxFileSize: Long)

final case class ExternalStorageConfig(endpoint: Uri, credentials: String)

final case class S3Config(endpoint: Uri, accessKey: Option[String], secretKey: Option[String]) {
  val endpointURI: URI = new URI(endpoint.toString)
  def endpointWithSubdomain(bucket: String): Uri =
    endpoint.copy(authority = endpoint.authority.copy(host = Host(s"$bucket.${endpoint.authority.host.toString()}")))

}

object TestsConfig {

  implicit val uriConverter: ConfigConvert[Uri] =
    ConfigConvert.viaString[Uri](catchReadError(s => Uri(s)), _.toString)



  import pureconfig.generic.auto._
  def load(config: Config): TestsConfig =
      ConfigSource.fromConfig(config).at("tests").loadOrThrow[TestsConfig]
}
