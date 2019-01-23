package ch.epfl.bluebrain.nexus.tests.iam.types

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._

final case class AclListing(_results: List[Acl], _total: Long)

final case class Acl(acl: List[AclEntry], _path: String, _rev: Long)

final case class AclEntry(identity: Identity, permissions: Set[String])

sealed trait Identity

case object Anonymous extends Identity

final case class User(realm: String, subject: String) extends Identity
final case class Authenticated(realm: String)         extends Identity
final case class Group(realm: String, group: String)  extends Identity

object AclListing {

  private implicit val config: Configuration = Configuration.default.withDiscriminator("@type")

  implicit val identityDecoder: Decoder[Identity]     = deriveDecoder[Identity]
  implicit val aclEntryDecoder: Decoder[AclEntry]     = deriveDecoder[AclEntry]
  implicit val aclDecoder: Decoder[Acl]               = deriveDecoder[Acl]
  implicit val aclListingDecoder: Decoder[AclListing] = deriveDecoder[AclListing]

}
