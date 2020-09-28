package ch.epfl.bluebrain.nexus.tests

import akka.http.scaladsl.model.HttpMethods.PUT
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, StatusCode}
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.test.Resources
import com.typesafe.scalalogging.Logger
import monix.bio.Task
import scala.concurrent.duration._

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
    ).onErrorRestartLoop((10, 10.seconds)) { (err, state, retry) =>
      // We have a retry here as the first thing we do with keycloak is getting a token
      // And without a warmup, we can get an UnexpectedConnectionClosureException
      // because Keycloak is still starting
      val (maxRetries, delay) = state
      if (maxRetries > 0)
        retry((maxRetries - 1, delay)).delayExecution(delay)
      else
        Task.raiseError(err)
    }.tapError { t =>
      Task { logger.error(s"Error while importing elasticsearch template", t) }
    }.map { res =>
      logger.info(s"Importing the elasticsearch template returned ${res.status}")
      res.status
    }
  }

}
