package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.concurrent.ThreadLocalRandom

import io.gatling.core.Predef._
import io.gatling.http.Predef._

class GetByTagSimulation extends BaseSimulation {

  val journeyDuration = config.fetchConfig.duration

  val project      = config.updateConfig.project
  val revisions    = config.updateConfig.revisions
  val revisionStep = config.updateConfig.revisionsStep
  val tags         = config.tagConfig.tags

  val scn = scenario("get by tag")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .tryMax(config.http.retries) {
      exec(
        http("list ${schema}")
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
    }

  setUp(scn.inject(atOnceUsers(config.fetchConfig.users)).protocols(httpConf))

}
