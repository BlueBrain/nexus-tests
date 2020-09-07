package ch.epfl.bluebrain.nexus.tests

import io.circe.optics.JsonPath.root

object Optics {

  object keycloak {
    val access_token = root.access_token.string
  }

  object error {
    val `@type` = root.`@type`.string
  }

}
