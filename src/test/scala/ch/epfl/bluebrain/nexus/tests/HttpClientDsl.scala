package ch.epfl.bluebrain.nexus.tests

import java.util.concurrent.ConcurrentHashMap

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.Identity.Anonymous
import com.typesafe.scalalogging.Logger
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

trait HttpClientDsl extends Matchers

object HttpClientDsl extends HttpClientDsl {

  private val logger = Logger[this.type]

  val baseUrl: Uri = Uri(s"http://${System.getProperty("delta:8080")}/v1")

  val tokensMap: ConcurrentHashMap[Identity, Authorization] = new ConcurrentHashMap[Identity, Authorization]

  private[tests] implicit class HttpClientOps(val httpClient: UntypedHttpClient[Task])
                                             (implicit materializer: Materializer) {

    def post[A](url: String, body: Json, identity: Identity)
               (assertResponse: (A, HttpResponse) => Assertion)
               (implicit um: FromEntityUnmarshaller[A]): Task[Assertion] =
      request(POST, url, Some(body), identity)(assertResponse)

    def put[A](url: String, body: Json, identity: Identity)
              (assertResponse: (A, HttpResponse) => Assertion)
              (implicit um: FromEntityUnmarshaller[A]): Task[Assertion] =
      request(PUT, url, Some(body), identity)(assertResponse)

    def patch[A](url: String, body: Json, identity: Identity)
                (assertResponse: (A, HttpResponse) => Assertion)
                (implicit um: FromEntityUnmarshaller[A]): Task[Assertion] =
      request(PATCH, url, Some(body), identity)(assertResponse)

    def get[A](url: String, identity: Identity)
              (assertResponse: (A, HttpResponse) => Assertion)
              (implicit um: FromEntityUnmarshaller[A]): Task[Assertion] =
      request(GET, url, None, identity)(assertResponse)

    def delete[A](url: String, identity: Identity)
                 (assertResponse: (A, HttpResponse) => Assertion)
                 (implicit um: FromEntityUnmarshaller[A]): Task[Assertion] =
      request(DELETE, url, None, identity)(assertResponse)

    def request[A](method: HttpMethod,
                   url: String,
                   body: Option[Json],
                   identity: Identity)
                  (assertResponse: (A, HttpResponse) => Assertion)
                  (implicit um: FromEntityUnmarshaller[A]): Task[Assertion] =
      httpClient(
        HttpRequest(
          method = method,
          uri = s"$baseUrl$url",
          headers = identity match {
            case Anonymous => Nil
            case _ => tokensMap.get(identity) :: Nil
          },
          entity = body.fold(HttpEntity.Empty)
          (j => HttpEntity(ContentTypes.`application/json`, j.noSpaces))
        )
      ).flatMap { res =>
        Task.deferFuture {
          um(res.entity)(global, materializer)
        }.map {
          assertResponse(_, res)
        }.onErrorHandleWith { e =>
          for {
            // Deserializing to case class may fail, json should
            // be fine in almost every case
            json <- Task.deferFuture {
              implicitly[FromEntityUnmarshaller[Json]].apply(res.entity)(global, materializer)
            }.onErrorHandle {
              _ => Json.Null
            }
            _   <- Task {
              logger.error(s"Status ${res.status}", e)
              logger.error(json.spaces2)
            }
          } yield {
            fail(s"Something went wrong while processing the response for url: ${method.value} $url with identity $identity", e)
          }
        }
      }
  }

}
