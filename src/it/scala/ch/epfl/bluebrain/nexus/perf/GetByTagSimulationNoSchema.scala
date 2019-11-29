package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.concurrent.ThreadLocalRandom

import io.gatling.core.Predef._
import io.gatling.http.Predef._

class GetByTagSimulationNoSchema extends BaseSimulation {

  val journeyDuration = config.fetch.duration

  val project      = config.update.project
  val revisions    = config.update.revisions
  val revisionStep = config.update.revisionsStep
  val tags         = config.tag.tags

  val scn = scenario("GetByTagSimulation")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .tryMax(config.http.retries) {
      exec(
        http("List Resources")
          .get(s"/resources/perftestorg/perftestproj$project/resource")
          check jsonPath("$.._total").ofType[Int].saveAs("search_total")
      ).during(journeyDuration)(
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
          http("Get Resource By Id And Tag")
            .get(s"/resources/perftestorg/perftestproj$project/resource/$${encodedId}?tag=v0.0.$${tag}")
        )
      )
    }

  setUp(scn.inject(atOnceUsers(config.fetch.users)).protocols(httpConf))

}
