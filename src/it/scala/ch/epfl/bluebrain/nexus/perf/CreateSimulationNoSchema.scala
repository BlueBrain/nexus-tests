package ch.epfl.bluebrain.nexus.perf

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.perf.data.generation.types.Settings
import ch.epfl.bluebrain.nexus.perf.data.generation.{ResourcesGenerator, Templates}
import io.circe.Json
import io.circe.parser.parse
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class CreateSimulationNoSchema extends BaseSimulation {

  private val projectNumber = config.createConfig.project.toString
  private val size          = config.createConfig.size
  private val parallelUsers = config.createConfig.parallelUsers

  private val map = Map[String, Uri](
    "person"                -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/person",
    "stimulusexperiment"    -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/stimulusexperiment",
    "trace"                 -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/trace",
    "tracegeneration"       -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/tracegeneration",
    "brainslicing"          -> "https://bluebrain.github.io/nexus/schemas/experiment/brainslicing",
    "patchedcell"           -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcell",
    "patchedcellcollection" -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcellcollection",
    "patchedslice"          -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcellslice",
    "protocol"              -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/protocol",
    "slice"                 -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/slice",
    "subject"               -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/subject",
    "wholecellpatchclamp"   -> "https://bluebrain.github.io/nexus/schemas/experiment/wholecellpatchclamp"
  )
  private val settings  = Settings(Uri("http://example.com/ids/"), map)
  private val templates = Templates(settings)

  private def feeder =
    ResourcesGenerator(templates, 1, size / 20, 20)
      .flatMap { instance =>
        if (instance.schema.toString == "https://bluebrain.github.io/nexus/schemas/experiment/wholecellpatchclamp") {
          Stream(
            Map(
              "payload"          -> instance.payload,
              "project"          -> projectNumber,
              "schema"           -> "resource",
              "schemaNonEncoded" -> "resource"
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
              "schema"           -> "resource",
              "schemaNonEncoded" -> "resource"
            ))
        }
      }

  private val scn = scenario("upload")
    .repeat(size / parallelUsers) {
      feed(feeder.iterator)
        .tryMax(config.http.retries) {
          exec(
            http("post to ${schemaNonEncoded}")
              .post("/resources/perftestorg/perftestproj${project}/${schema}")
              .body(StringBody("${payload}"))
              .header("Content-Type", "application/json")
              .check(status.in(201, 409))
          )
        }
    }

  setUp(scn.inject(atOnceUsers(parallelUsers)).protocols(httpConf))
}
