package ch.epfl.bluebrain.nexus.perf

import java.net.URLEncoder
import java.util.concurrent.ThreadLocalRandom

import io.circe.parser.parse
import io.gatling.core.Predef._
import io.gatling.http.Predef._

class FetchAttachmentSimulation extends BaseSimulation {

  val project = config.attachmentsConfig.project

  val scn = scenario("FetchAttachmentSimulation")
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
          http("Get Resource By Id")
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
          http("Download Attachment")
            .get("${downloadUrl}")
        )
    )

  setUp(scn.inject(atOnceUsers(config.attachmentsConfig.parallelUsers)).protocols(httpConf))
}
