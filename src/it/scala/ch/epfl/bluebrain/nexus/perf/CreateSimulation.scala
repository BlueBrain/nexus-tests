package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.perf.data.generation.types.Settings
import ch.epfl.bluebrain.nexus.perf.data.generation.{ResourcesGenerator, Templates}
import io.circe.Json
import io.circe.parser.parse
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class CreateSimulation extends BaseSimulation {

  private val projectNumber = config.create.project.toString
  private val size          = config.create.size
  private val parallelUsers = config.create.parallelUsers
  private val settings      = Settings(Uri("http://example.com/ids/"), map)
  private val templates     = Templates(settings)

  private def feeder =
    ResourcesGenerator(templates, 1, size / 20, 20)
      .flatMap { instance =>
        if (instance.schema.toString == "https://bluebrain.github.io/nexus/schemas/experiment/wholecellpatchclamp") {
          Stream(
            Map(
              "payload"          -> instance.payload,
              "project"          -> projectNumber,
              "schema"           -> URLEncoder.encode(instance.schema.toString, "UTF-8"),
              "schemaNonEncoded" -> instance.schema.toString
            ),
            Map(
              "payload" -> parse(instance.payload).right.get.mapObject { obj =>
                obj.add("@id", Json.fromString(s"${obj("@id").get.asString.get}/resource"))
              }.noSpaces,
              "project"          -> projectNumber,
              "schema"           -> "resource",
              "schemaNonEncoded" -> "resource"
            )
          )
        } else {
          Stream(
            Map(
              "payload"          -> instance.payload,
              "project"          -> projectNumber,
              "schema"           -> URLEncoder.encode(instance.schema.toString, "UTF-8"),
              "schemaNonEncoded" -> instance.schema.toString
            )
          )
        }
      }

  private val scn = scenario("CreateSimulation")
    .repeat(size / parallelUsers) {
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
