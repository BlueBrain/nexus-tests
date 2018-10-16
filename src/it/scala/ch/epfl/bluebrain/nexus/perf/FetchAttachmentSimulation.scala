package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.concurrent.ThreadLocalRandom

import ch.epfl.bluebrain.nexus.perf.config.Settings
import com.typesafe.config.ConfigFactory
import io.circe.parser.parse
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class FetchAttachmentSimulation extends Simulation {

  val config = new Settings(ConfigFactory.parseResources("perf-tests.conf").resolve()).appConfig

  val schemas = List(
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/wholecellpatchclamp"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/subject"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/slice"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/protocol"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcellslice"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcellcollection"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/patchedcell"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/experiment/brainslicing"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/tracegeneration"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/trace"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/stimulusexperiment"),
    Map("schema" -> "https://bluebrain.github.io/nexus/schemas/neurosciencegraph/core/person")
  )

  val schemasFeeder = schemas.toArray.circular

  val project = config.attachmentsConfig.project

  val httpConf = http
    .baseUrl(config.kg.base.toString) // Here is the root for all relative URLs
    .authorizationHeader(s"Bearer ${config.http.token}")

  val scn = scenario("fetchAttachment")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .during(config.attachmentsConfig.duration)(
      exec { session =>
        val rnd = ThreadLocalRandom
          .current()
          .nextInt(config.attachmentsConfig.instances) + 1
        val s = session("schema").as[String]
        session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8"))

      }.exec(
          http("fetch from ${schema}")
            .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}")
            .check(bodyString.saveAs("savedPayload"))
        )
        .exec { session =>
          val json = parse(session("savedPayload").as[String]).right.get

          val distributions = json.hcursor
            .downField("_distribution")
            .values
            .get

          val downloadUrls = distributions.map(_.hcursor.downField("_downloadURL").as[String].right.get).toList

          session.set("downloadUrl",
                      downloadUrls(
                        ThreadLocalRandom
                          .current()
                          .nextInt(downloadUrls.size)))
        }
        .exec(
          http("get attachment from ${schema}")
            .get("${downloadUrl}")
        )
    )

  setUp(scn.inject(atOnceUsers(config.attachmentsConfig.parallelUsers)).protocols(httpConf))
}
