package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.perf.config.Settings
import ch.epfl.bluebrain.nexus.perf.data.generation.{ResourcesGenerator, Templates}
import ch.epfl.bluebrain.nexus.perf.data.generation.types.{Settings => GenerationSettings}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import io.circe.parser.parse
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class UploadSimulation extends Simulation {

  private val config        = new Settings(ConfigFactory.parseResources("perf-tests.conf").resolve()).appConfig
  private val projectNumber = config.uploadConfig.project.toString
  private val size          = config.uploadConfig.size
  private val parallelUsers = config.uploadConfig.parallelUsers

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
  private val settings  = GenerationSettings(Uri("http://example.com/ids/"), map)
  private val templates = Templates(settings)

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
              },
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
            ))
        }
      }

  private val httpConf = http
    .baseUrl(config.kg.base.toString) // Here is the root for all relative URLs
    .authorizationHeader(s"Bearer ${config.http.token}")

  private val scn = scenario("upload")
    .repeat(size / parallelUsers) {
      feed(feeder.iterator)
        .exec(
          http("post to ${schemaNonEncoded}")
            .post("/resources/perftestorg/perftestproj${project}/${schema}")
            .body(StringBody("${payload}"))
            .header("Content-Type", "application/json")
            .check(status.in(201, 409))
        )
    }

  setUp(scn.inject(atOnceUsers(parallelUsers)).protocols(httpConf))
}
