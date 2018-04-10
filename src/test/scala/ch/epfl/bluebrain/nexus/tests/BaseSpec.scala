package ch.epfl.bluebrain.nexus.tests

import java.util.regex.Pattern.quote

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model.Uri.{Path => AkkaPath}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.ActorMaterializer
import akka.util.ByteString
import ch.epfl.bluebrain.nexus.commons.http.HttpClient
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.config.Settings
import com.typesafe.config.ConfigFactory
import io.circe.Json
import io.circe.parser._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Assertion, Matchers, WordSpecLike}

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}

class BaseSpec
    extends WordSpecLike
    with Matchers
    with ScalatestRouteTest
    with ScalaFutures
    with Resources
    with Randomness {

  private[tests] val config                        = new Settings(ConfigFactory.parseResources("test-app.conf").resolve()).appConfig
  private[tests] val credGroup                     = HttpCredentials.createOAuth2BearerToken(config.iam.groupToken)
  private[tests] val credUser                      = HttpCredentials.createOAuth2BearerToken(config.iam.userToken)
  private[tests] val headersGroup: Seq[HttpHeader] = Seq(Authorization(credGroup))
  private[tests] val headersUser: Seq[HttpHeader]  = Seq(Authorization(credUser))
  private[tests] val errorCtx                      = Map(quote("{error-context}") -> config.prefixes.errorContext.toString)
  private[tests] val resourceCtx                   = Map(quote("{success-context}") -> config.prefixes.coreContext.toString)
  private[tests] val resourceIamCtx                = Map(quote("{success-iam-context}") -> config.iam.coreContext.toString)

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(config.http.patienceConfig, 300 millis)

  private implicit val ec: ExecutionContextExecutor = system.dispatcher
  private implicit val mt: ActorMaterializer        = ActorMaterializer()

  private[tests] implicit val cl: UntypedHttpClient[Future] = HttpClient.akkaHttpClient

  private[tests] implicit def toPath(str: String): AkkaPath = AkkaPath(str)

  private[tests] implicit class JsonSyntax(json: Json) {
    def toEntity: RequestEntity = HttpEntity(ContentTypes.`application/json`, json.noSpaces)
  }

  private[tests] implicit class HttpResponseSyntax(value: Future[HttpResponse])(
      implicit um: FromEntityUnmarshaller[Json]) {
    def mapJson(body: (Json, HttpResponse) => Assertion): Assertion =
      whenReady(value)(res => um(res.entity).map(json => body(json, res)).futureValue)

    def mapResp(body: (HttpResponse) => Assertion): Assertion =
      whenReady(value)(body(_))

  }

  private[tests] implicit class RequestEntitySyntax(entity: RequestEntity) {
    def toJson: Option[Json] =
      entity.dataBytes.runFold(ByteString(""))(_ ++ _).map(_.utf8String).map(parse).futureValue.toOption
  }

}
