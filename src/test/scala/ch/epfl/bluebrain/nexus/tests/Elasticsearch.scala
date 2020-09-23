package ch.epfl.bluebrain.nexus.tests

import akka.http.scaladsl.model.HttpMethods.PUT
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, StatusCode}
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.test.Resources
import com.typesafe.scalalogging.Logger
import monix.bio.Task

object Elasticsearch extends Resources {

  private val logger = Logger[this.type]

  private val elasticUrl = s"http://${System.getProperty("elasticsearch:9200")}"

  def createTemplate(implicit cl: UntypedHttpClient[Task]): Task[StatusCode] = {
    logger.info("Creating template for Elasticsearch indices")

    val json = jsonContentOf("/elasticsearch/template.json")

    cl(
      HttpRequest(
        method = PUT,
        uri = s"$elasticUrl/_index_template/test_template",
        entity = HttpEntity(ContentTypes.`application/json`, json.noSpaces)
      )
    ).tapError { t =>
      Task { logger.error(s"Error while importing elasticsearch template", t) }
    }.map { res =>
      logger.info(s"Importing the elasticsearch template returned ${res.status}")
      res.status
    }
  }

}
