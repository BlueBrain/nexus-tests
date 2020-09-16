package ch.epfl.bluebrain.nexus.tests

import java.util.concurrent.ConcurrentHashMap

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Accept, Authorization}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.tests.Identity.Anonymous
import com.typesafe.scalalogging.Logger
import fs2._
import io.circe.Json
import monix.bio.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers

trait HttpClientDsl extends Matchers

object HttpClientDsl extends HttpClientDsl {

  private val logger = Logger[this.type]

  val deltaUrl: Uri = Uri(s"http://${System.getProperty("delta:8080")}/v1")

  val tokensMap: ConcurrentHashMap[Identity, Authorization] = new ConcurrentHashMap[Identity, Authorization]

  val jsonHeaders: Seq[HttpHeader] = Accept(MediaTypes.`application/json`) :: Nil

  private[tests] implicit class HttpClientOps(val httpClient: UntypedHttpClient[Task])
                                             (implicit materializer: Materializer) {

    def post[A](url: String, body: Json, identity: Identity)
               (assertResponse: (A, HttpResponse) => Assertion)
               (implicit um: FromEntityUnmarshaller[A],
                extraHeaders: Seq[HttpHeader] = jsonHeaders): Task[Assertion] =
      requestAssert(POST, url, Some(body), identity)(assertResponse)

    def put[A](url: String, body: Json, identity: Identity)
              (assertResponse: (A, HttpResponse) => Assertion)
              (implicit um: FromEntityUnmarshaller[A],
               extraHeaders: Seq[HttpHeader] = jsonHeaders): Task[Assertion] =
      requestAssert(PUT, url, Some(body), identity)(assertResponse)

    def patch[A](url: String, body: Json, identity: Identity)
                (assertResponse: (A, HttpResponse) => Assertion)
                (implicit um: FromEntityUnmarshaller[A],
                 extraHeaders: Seq[HttpHeader] = jsonHeaders): Task[Assertion] =
      requestAssert(PATCH, url, Some(body), identity)(assertResponse)

    def get[A](url: String, identity: Identity)
              (assertResponse: (A, HttpResponse) => Assertion)
              (implicit um: FromEntityUnmarshaller[A],
               extraHeaders: Seq[HttpHeader] = jsonHeaders): Task[Assertion] =
      requestAssert(GET, url, None, identity)(assertResponse)

    def delete[A](url: String, identity: Identity)
                 (assertResponse: (A, HttpResponse) => Assertion)
                 (implicit um: FromEntityUnmarshaller[A],
                  extraHeaders: Seq[HttpHeader] = jsonHeaders): Task[Assertion] =
      requestAssert(DELETE, url, None, identity)(assertResponse)

    def requestAssert[A](method: HttpMethod,
                         url: String,
                         body: Option[Json],
                         identity: Identity)
                        (assertResponse: (A, HttpResponse) => Assertion)
                        (implicit um: FromEntityUnmarshaller[A],
                        extraHeaders: Seq[HttpHeader] = jsonHeaders): Task[Assertion] = {
      def onFail(e: Throwable) = fail(s"Something went wrong while processing the response for url: ${method.value} $url with identity $identity", e)

      request(
        method,
        s"$deltaUrl$url",
        body,
        identity,
        assertResponse,
        onFail
      )
    }

    def request[A, B](method: HttpMethod,
                      url: String,
                      body: Option[Json],
                      identity: Identity,
                      f: (A, HttpResponse) => B,
                      handleError: Throwable => B)
                      (implicit um: FromEntityUnmarshaller[A],
                      extraHeaders: Seq[HttpHeader] = jsonHeaders): Task[B] =
      httpClient(
        HttpRequest(
          method = method,
          uri = url,
          headers = identity match {
            case Anonymous => extraHeaders
            case _ => tokensMap.get(identity) +: extraHeaders
          },
          entity = body.fold(HttpEntity.Empty)
          (j => HttpEntity(ContentTypes.`application/json`, j.noSpaces))
        )
      ).flatMap { res =>
        Task.deferFuture {
          um(res.entity)(global, materializer)
        }.map {
          f(_, res)
        }.onErrorHandleWith { e =>
          for {
            // Deserializing to case class may fail, json should
            // be fine in almost every case
            json <- Task.deferFuture {
              implicitly[FromEntityUnmarshaller[Json]].apply(res.entity)(global, materializer)
            }.onErrorHandle {
              _ =>
                logger.error("We can't even deserialize in json")
                Json.Null
            }
            _   <- Task {
              logger.error(s"Status ${res.status}", e)
              logger.error(json.spaces2)
            }
          } yield {
            handleError(e)
          }
        }
      }

    def stream[A, B](url: String,
                     nextUrl: A => Option[String],
                     lens: A => B,
                     identity: Identity)
                    (implicit um: FromEntityUnmarshaller[A],
                     extraHeaders: Seq[HttpHeader] = jsonHeaders): Stream[Task, B] = {
      def onFail(e: Throwable) = throw new IllegalStateException(s"Something went wrong while processing the response for url: $url with identity $identity", e)
      Stream.unfoldLoopEval[Task, String, B](s"$deltaUrl$url") { currentUrl =>
        request[A, A](
          GET,
          currentUrl,
          None,
          identity,
          (a: A, _: HttpResponse) => a,
          onFail
        ).map { a =>
          (lens(a), nextUrl(a))
        }
      }
    }
  }

}
