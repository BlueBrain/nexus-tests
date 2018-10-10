package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.concurrent.ThreadLocalRandom

import ch.epfl.bluebrain.nexus.perf.config.Settings
import com.typesafe.config.ConfigFactory
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class GetByTagSimulation extends Simulation {

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
  ).toArray.circular

  val httpConf = http
    .baseUrl(config.kg.base.toString) // Here is the root for all relative URLs
    .authorizationHeader(s"Bearer ${config.http.token}")
  val journeyDuration = config.fetchConfig.duration

  val project      = config.updateConfig.project
  val revisions    = config.updateConfig.revisions
  val revisionStep = config.updateConfig.revisionsStep
  val tags         = config.tagConfig.tags

  val scn = scenario("get by tag")
    .feed(schemas)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .exec(http("list ${schema}")
      .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}")
      check jsonPath("$.._total").ofType[Int].saveAs("search_total"))
    .during(journeyDuration)(
      exec { session =>
        val rnd = ThreadLocalRandom
          .current()
          .nextInt(Math.min(revisions / revisionStep, session("search_total").as[Int])) + 1
        val s = session("schema").as[String]
        val tag = ThreadLocalRandom
          .current()
          .nextInt(tags) + 1
        session.set("encodedId", URLEncoder.encode(s"$s/ids/$rnd", "UTF-8")).set("tag", tag)

      }.exec(
        http("fetch by tag ${schema}")
          .get(s"/resources/perftestorg/perftestproj$project/$${encodedSchema}/$${encodedId}?tag=v0.0.$${tag}")
      )
    )

  setUp(scn.inject(atOnceUsers(config.fetchConfig.users)).protocols(httpConf))

}
