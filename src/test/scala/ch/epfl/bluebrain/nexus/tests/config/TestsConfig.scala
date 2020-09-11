package ch.epfl.bluebrain.nexus.tests.config

import java.net.URI
import java.util.regex.Pattern.quote

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Host
import ch.epfl.bluebrain.nexus.tests.Realm

import scala.concurrent.duration.FiniteDuration

case class TestsConfig(deltaUri: Uri,
                             realmUri: Uri,
                             patience: FiniteDuration) {

  def realmSuffix(realm: Realm) = s"$realmUri/${realm.name}"
}

final case class PrefixesConfig(iamCoreContext: Uri,
                                coreContext: Uri,
                                standardsContext: Uri,
                                linksContext: Uri,
                                searchContext: Uri,
                                distributionContext: Uri,
                                errorContext: Uri) {

  def coreContextMap = Map(quote("{success-context}") -> coreContext.toString)
}

final case class StorageConfig(s3: S3Config, external: ExternalStorageConfig, maxFileSize: Long)

final case class ExternalStorageConfig(endpoint: Uri, credentials: String)

final case class S3Config(endpoint: Uri, accessKey: Option[String], secretKey: Option[String]) {
  val endpointURI: URI = new URI(endpoint.toString)
  def endpointWithSubdomain(bucket: String): Uri =
    endpoint.copy(authority = endpoint.authority.copy(host = Host(s"$bucket.${endpoint.authority.host.toString()}")))

}

