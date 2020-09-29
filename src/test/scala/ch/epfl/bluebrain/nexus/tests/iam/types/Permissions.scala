package ch.epfl.bluebrain.nexus.tests.iam.types

import com.github.ghik.silencer.silent
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder

final case class Permissions(permissions: Set[Permission], _rev: Long)

object Permissions {
  @silent
  implicit val identityDecoder: Decoder[Permissions] = {
    implicit val config: Configuration = Configuration.default
    deriveConfiguredDecoder[Permissions]
  }
}
