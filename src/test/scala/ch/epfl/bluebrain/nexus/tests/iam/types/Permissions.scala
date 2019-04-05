package ch.epfl.bluebrain.nexus.tests.iam.types

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder

final case class Permissions(permissions: Set[String], _rev: Long)

object Permissions {
  private implicit val config: Configuration         = Configuration.default
  implicit val identityDecoder: Decoder[Permissions] = deriveDecoder[Permissions]
}
