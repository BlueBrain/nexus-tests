package ch.epfl.bluebrain.nexus.tests.config

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.commons.types.search.Pagination
import ch.epfl.bluebrain.nexus.tests.config.AppConfig._

import scala.concurrent.duration.Duration

/**
  * Case class which aggregates the configuration parameters
  *
  * @param http       http settings
  * @param kg         kg connection settings
  * @param admin      admin connection settings
  * @param iam        IAM connection settings
  * @param prefixes   the collection of prefixes used throughout the service
  * @param pagination Routes pagination settings
  */
final case class AppConfig(http: HttpConfig,
                           kg: KgConfig,
                           admin: AdminConfig,
                           iam: IamConfig,
                           prefixes: PrefixesConfig,
                           pagination: PaginationConfig)

object AppConfig {

  final case class HttpConfig(patienceConfig: Duration)

  final case class KgConfig(uri: Uri)

  final case class AdminConfig(uri: Uri, attachmentSize: Long)

  final case class IamConfig(uri: Uri, groupToken: String, userToken: String, userSub: String, coreContext: Uri)

  final case class PrefixesConfig(coreContext: Uri,
                                  standardsContext: Uri,
                                  linksContext: Uri,
                                  searchContext: Uri,
                                  distributionContext: Uri,
                                  errorContext: Uri)

  final case class PaginationConfig(from: Long, size: Int, maxSize: Int) {
    val pagination: Pagination = Pagination(from, size)
  }

}
