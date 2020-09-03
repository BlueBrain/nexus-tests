package ch.epfl.bluebrain.nexus.tests.iam.types

import com.github.ghik.silencer.silent
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._

final case class AclListing(_results: List[Acl], _total: Long)

final case class Acl(acl: List[AclEntry], _path: String, _rev: Long)

final case class AclEntry(identity: Identity, permissions: Set[String])

sealed trait Identity

case object Anonymous extends Identity

final case class AclUser(realm: String, subject: String) extends Identity
final case class Authenticated(realm: String)            extends Identity
final case class Group(realm: String, group: String)     extends Identity

object AclListing {

  @silent
  private implicit val config: Configuration = Configuration.default.withDiscriminator("@type")

  implicit val identityDecoder: Decoder[Identity]     = deriveConfiguredDecoder[Identity]
  implicit val aclEntryDecoder: Decoder[AclEntry]     = deriveConfiguredDecoder[AclEntry]
  implicit val aclDecoder: Decoder[Acl]               = deriveConfiguredDecoder[Acl]
  implicit val aclListingDecoder: Decoder[AclListing] = deriveConfiguredDecoder[AclListing]

}
