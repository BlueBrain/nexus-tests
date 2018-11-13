package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.concurrent.ThreadLocalRandom

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.http.Predef._

class TagSimulationNoSchema extends BaseSimulation {

  val project      = config.updateConfig.project
  val revisions    = config.updateConfig.revisions
  val revisionStep = config.updateConfig.revisionsStep
  val tags         = config.tagConfig.tags

  val repeatCountExpression: Expression[Int] = { session =>
    Math.min(revisions / revisionStep, session("search_total").as[Int])
  }

  val scn = scenario("TagSimulationNoSchema")
    .feed(schemasFeeder)
    .exec { session =>
      val s = session("schema").as[String]
      session.set("encodedSchema", URLEncoder.encode(s, "UTF-8"))
    }
    .tryMax(config.http.retries) {
      exec(
        http("List Resources")
          .get(s"/resources/perftestorg/perftestproj$project/resource")
          check jsonPath("$.._total").ofType[Int].saveAs("search_total"))
        .repeat(repeatCountExpression, "instanceNumber")(
          exec { session =>
            val s              = session("schema").as[String]
            val instanceNumber = session("instanceNumber").as[Int] + 1
            session
              .set("encodedId", URLEncoder.encode(s"$s/ids/$instanceNumber", "UTF-8"))
          }.repeat(tags, "tagCounter")(
            exec { session =>
              session.set("tag", session("tagCounter").as[Int] + 1)
            }.exec(
                http("Get Resource By Id")
                  .get(s"/resources/perftestorg/perftestproj$project/resource/$${encodedId}")
                  .check(jsonPath("$.._rev")
                    .ofType[Int]
                    .saveAs("currentRevision"))
              )
              .exec { session =>
                val rnd = ThreadLocalRandom
                  .current()
                  .nextInt(session("currentRevision").as[Int]) + 1
                session.set("tagRevision", rnd)
              }
              .exec(
                http("Tag Resource")
                  .put(
                    s"/resources/perftestorg/perftestproj$project/resource/$${encodedId}/tags?rev=$${currentRevision}")
                  .body(StringBody("""{"tag": "v0.0.${tag}","rev": ${tagRevision}} """))
                  .header("Content-Type", "application/json")
              )
          )
        )
    }

  setUp(scn.inject(atOnceUsers(config.fetchConfig.users)).protocols(httpConf))
}
