package ch.epfl.bluebrain.nexus.tests.kg

import java.util.regex.Pattern.quote

import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.config.TestsConfig
import io.circe.Json
import org.scalatest.matchers.should.Matchers

class KgDsl(config: TestsConfig) extends Randomness with Resources with Matchers {

  def projectJson(path: String = "/kg/projects/project.json",
                  name: String = genString()): Json = {
    val base = s"${config.deltaUri.toString()}/resources/$name/_/"
    val rep  = Map(quote("{name}") -> name, quote("{base}") -> base)
    jsonContentOf(path, rep)
  }


}
