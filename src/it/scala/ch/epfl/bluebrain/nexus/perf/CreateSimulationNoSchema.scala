package ch.epfl.bluebrain.nexus.perf

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.perf.data.generation.types.Settings
import ch.epfl.bluebrain.nexus.perf.data.generation.{ResourcesGenerator, Templates}
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class CreateSimulationNoSchema extends BaseSimulation {

  private val projectNumber = config.createConfig.project.toString
  private val size          = config.createConfig.size
  private val parallelUsers = config.createConfig.parallelUsers
  private val settings      = Settings(Uri("http://example.com/ids/"), map)
  private val templates     = Templates(settings)

  private def feeder =
    ResourcesGenerator(templates, 1, size / 20, 20)
      .map { instance =>
        Map(
          "payload"          -> instance.payload,
          "project"          -> projectNumber,
          "schema"           -> "resource",
          "schemaNonEncoded" -> "resource"
        )
      }

  private val scn = scenario("CreateSimulationNoSchema")
    .repeat(size / parallelUsers) {
      feed(feeder.iterator)
        .tryMax(config.http.retries) {
          exec(
            http("Create Resource (no validation)")
              .post("/resources/perftestorg/perftestproj${project}/${schema}")
              .body(StringBody("${payload}"))
              .header("Content-Type", "application/json")
              .check(status.in(201, 409))
          )
        }
    }

  setUp(scn.inject(atOnceUsers(parallelUsers)).protocols(httpConf))
}
