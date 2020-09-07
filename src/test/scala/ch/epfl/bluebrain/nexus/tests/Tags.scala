package ch.epfl.bluebrain.nexus.tests

import org.scalatest.Tag

trait Tags

/**
 * Scalatest tags
 */
object Tags extends Tags {

  object IamTag extends Tag("Iam")
  object RealmsTag extends Tag("Realms")
  object PermissionsTag extends Tag("Permissions")
  object AclsTag extends Tag("Acls")

  object ToMigrateTag extends Tag("ToMigrate")
}
