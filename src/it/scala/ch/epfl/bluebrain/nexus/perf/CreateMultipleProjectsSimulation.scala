package ch.epfl.bluebrain.nexus.perf
import java.net.URLEncoder

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.perf.data.generation.{ResourcesGenerator, Templates}
import ch.epfl.bluebrain.nexus.perf.data.generation.types.Settings
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class CreateMultipleProjectsSimulation extends BaseSimulation {

  private val numProjects         = config.multipleProjectsConfig.projects
  private val instancesPerProject = config.multipleProjectsConfig.instancesPerProject

  private val settings  = Settings(Uri("http://example.com/ids/"), map)
  private val templates = Templates(settings)

  private val parallelUsers = config.multipleProjectsConfig.parallelUsers
  private val totalSize     = numProjects * instancesPerProject

  private def feeder =
    (1 to numProjects).toStream
      .flatMap(projectNumber => ResourcesGenerator(templates, 1, instancesPerProject / 20, 20).map((projectNumber, _)))
      .map {
        case (projectNumber, instance) =>
          Map[String, Any](
            "payload"          -> instance.payload,
            "project"          -> projectNumber,
            "schema"           -> URLEncoder.encode(instance.schema.toString, "UTF-8"),
            "schemaNonEncoded" -> instance.schema.toString
          )
      }

  private val scn = scenario("CreateMultipleProjectsSimulation")
    .repeat(totalSize / parallelUsers) {
      feed(feeder.iterator)
        .tryMax(config.http.retries) {
          exec(
            http("Create Resource")
              .post("/resources/perftestorg/perftestproj${project}/${schema}")
              .body(StringBody("${payload}"))
              .header("Content-Type", "application/json")
              .check(status.in(201, 409))
          )
        }
    }

  setUp(scn.inject(atOnceUsers(parallelUsers)).protocols(httpConf))

}
