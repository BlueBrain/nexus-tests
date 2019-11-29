package ch.epfl.bluebrain.nexus.perf

import ch.epfl.bluebrain.nexus.commons.test.Resources
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class ElasticSearchSimulation extends BaseSimulation with Resources {

  val queries = jsonContentOf("/es-queries.json").hcursor
    .downField("queries")
    .values
    .get
    .map(json => Map("query" -> json.noSpaces))
    .toArray
    .random

  val project = config.esSearch.project

  val scn = scenario("ElasticSearchSimulation")
    .feed(queries)
    .during(config.esSearch.duration) {
      exec(
        http("ElasticSearch Query")
          .post(s"/views/perftestorg/perftestproj$project/nxv:defaultElasticIndex/_search")
          .body(StringBody("${query}"))
          .asJson
      )
    }

  setUp(
    scn
      .inject(
        rampConcurrentUsers(0) to config.esSearch.parallelUsers during (1 minutes),
        constantConcurrentUsers(config.esSearch.parallelUsers) during config.esSearch.duration
      )
      .protocols(httpConf)
  )
}
