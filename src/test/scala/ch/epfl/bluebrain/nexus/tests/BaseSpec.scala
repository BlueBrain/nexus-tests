package ch.epfl.bluebrain.nexus.tests

import java.util.regex.Pattern.quote

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model.HttpMethods.PATCH
import akka.http.scaladsl.model.Uri.{Path => AkkaPath}
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.model.{RequestEntity, StatusCodes, HttpRequest => Req, _}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.ActorMaterializer
import akka.util.ByteString
import ch.epfl.bluebrain.nexus.commons.http.HttpClient
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.config.Settings
import com.typesafe.config.ConfigFactory
import io.circe.Json
import io.circe.parser._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest._

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}

class BaseSpec
    extends WordSpecLike
    with BeforeAndAfterAll
    with Matchers
    with ScalatestRouteTest
    with ScalaFutures
    with Resources
    with Randomness
    with OptionValues {

  private[tests] val config                        = new Settings(ConfigFactory.parseResources("test-app.conf").resolve()).appConfig
  private[tests] val credGroup                     = HttpCredentials.createOAuth2BearerToken(config.iam.groupToken)
  private[tests] val credUser                      = HttpCredentials.createOAuth2BearerToken(config.iam.userToken)
  private[tests] val headersGroup: Seq[HttpHeader] = Seq(Authorization(credGroup))
  private[tests] val headersUser: Seq[HttpHeader]  = Seq(Authorization(credUser))
  private[tests] val errorCtx                      = Map(quote("{error-context}") -> config.prefixes.errorContext.toString)
  private[tests] val resourceCtx                   = Map(quote("{success-context}") -> config.prefixes.coreContext.toString)
  private[tests] val resourceIamCtx                = Map(quote("{success-iam-context}") -> config.iam.coreContext.toString)
  private[tests] val adminBase                     = config.admin.uri
  private[tests] val iamBase                       = config.iam.uri
  private[tests] val kgBase                        = config.kg.uri
  private[tests] val replSub                       = Map(quote("{sub}") -> config.iam.userSub)

  override def beforeAll(): Unit = {
    super.beforeAll()
    val _ = cleanAcls
  }
  def cleanAcls = {

    cl(Req(uri = s"$iamBase/acls/*/*?parents=true", headers = headersGroup)).mapJson { (json, result) =>
      result.status shouldEqual StatusCodes.OK
      val permissions =
        json.hcursor.get[Vector[Json]]("acl").toOption.value.foldLeft(Set.empty[(String, Set[String])]) {
          case (acc, j) =>
            j.hcursor.downField("identity").get[String]("sub") match {
              case Right(s) if s == config.iam.userSub =>
                acc + (j.hcursor.get[String]("path").toOption.value ->
                  j.hcursor.get[Set[String]]("permissions").toOption.value)
              case _ => acc
            }
        }

      permissions.foreach {
        case (path, perms) =>
          val entity =
            jsonContentOf("/iam/patch-single.json", replSub + (quote("{perms}") -> perms.mkString("""",""""))).toEntity
          cl(Req(PATCH, s"$iamBase/acls$path", headersGroup, entity)).mapJson { (json, result) =>
            result.status shouldEqual StatusCodes.OK
            json shouldEqual jsonContentOf("/iam/patch-response.json", replSub ++ resourceIamCtx)
          }
      }

      result.status shouldEqual StatusCodes.OK

    }
  }

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(config.http.patienceConfig, 300 millis)

  private implicit val ec: ExecutionContextExecutor = system.dispatcher
  private implicit val mt: ActorMaterializer        = ActorMaterializer()

  private[tests] implicit val cl: UntypedHttpClient[Future] = HttpClient.akkaHttpClient

  private[tests] implicit def toPath(str: String): AkkaPath = AkkaPath(str)

  private[tests] implicit class JsonSyntax(json: Json) {
    def toEntity: HttpEntity.Strict = HttpEntity(ContentTypes.`application/json`, json.noSpaces)

    def getString(field: String): String   = json.asObject.flatMap(_(field)).flatMap(_.asString).value
    def getLong(field: String): Long       = json.asObject.flatMap(_(field)).flatMap(_.asNumber).flatMap(_.toLong).value
    def getBoolean(field: String): Boolean = json.asObject.flatMap(_(field)).flatMap(_.asBoolean).value
    def getJson(field: String): Json       = json.asObject.flatMap(_(field)).value
    def getArray(field: String): Seq[Json] = json.asObject.flatMap(_(field)).flatMap(_.asArray).value

    def updateField(field: String, value: String): Json = json.mapObject(_.add(field, Json.fromString(value)))
    def removeField(field: String): Json                = json.mapObject(_.remove(field))
  }

  private[tests] implicit class HttpResponseSyntax(value: Future[HttpResponse]) {

    def mapJson(body: (Json, HttpResponse) => Assertion)(implicit um: FromEntityUnmarshaller[Json]): Assertion =
      whenReady(value)(res => um(res.entity).map(json => body(json, res)).futureValue)

    def mapString(body: (String, HttpResponse) => Assertion)(implicit um: FromEntityUnmarshaller[String]): Assertion =
      whenReady(value)(res => um(res.entity).map(s => body(s, res)).futureValue)

    def getJson[A](handler: Json => A)(implicit um: FromEntityUnmarshaller[Json]): A = {
      whenReady(value) { res =>
        um(res.entity).map(handler(_)).futureValue
      }
    }

    def mapResp(body: HttpResponse => Assertion): Assertion =
      whenReady(value)(body(_))

  }

  private[tests] implicit class RequestEntitySyntax(entity: RequestEntity) {
    def toJson: Option[Json] =
      entity.dataBytes.runFold(ByteString(""))(_ ++ _).map(_.utf8String).map(parse).futureValue.toOption
  }
  private[tests] val startPool = Vector.range('a', 'z')
  private[tests] val pool      = Vector.range('a', 'z') ++ Vector.range('0', '9') :+ '_' :+ '-'

  private[tests] def randomProjectPrefix = genString(1, startPool) + genString(genInt(10), pool)

  private[tests] def genId(length: Int = 15): String =
    genString(length = length, Vector.range('a', 'z') ++ Vector.range('0', '9'))

  private[tests] def projectReqEntity(
      path: String = "/admin/projects/create.json",
      nxv: String = randomProjectPrefix,
      person: String = randomProjectPrefix,
      name: String = genString(),
      base: String = s"${config.admin.uri.toString()}/${genString()}"): RequestEntity = {
    val rep = Map(quote("{nxv-prefix}") -> nxv,
                  quote("{person-prefix}") -> person,
                  quote("{name}")          -> name,
                  quote("{base}")          -> base)
    jsonContentOf(path, rep).toEntity
  }

  private[tests] def orgReqEntity(name: String = genString()): RequestEntity = {
    val rep = Map(quote("{name}") -> name)
    jsonContentOf("/admin/orgs/payload.json", rep).toEntity
  }

  def kgProjectReqEntity(path: String = "/kg/projects/project.json",
                         name: String = genString(),
                         base: String = s"${config.kg.uri.toString()}/resources/${genString()}/"): RequestEntity = {
    val rep = Map(quote("{name}") -> name, quote("{base}") -> base)
    jsonContentOf(path, rep).toEntity
  }

  private[tests] def createRespJson(id: String, rev: Long, tpe: String = "projects"): Json = {
    val resp = resourceCtx ++ Map(quote("{base}") -> config.admin.uri.toString(),
                                  quote("{id}")        -> id,
                                  quote("{type}")      -> tpe,
                                  quote(""""{rev}"""") -> rev.toString)
    jsonContentOf("/admin/response.json", resp)
  }

  private[tests] def validateAdminResource(json: Json,
                                           tpe: String,
                                           idPrefix: String,
                                           id: String,
                                           name: String,
                                           rev: Long,
                                           label: String,
                                           deprecated: Boolean = false) = {
    json.getString("@context") shouldEqual config.prefixes.coreContext.toString()
    json.getString("@id") shouldEqual s"${config.admin.uri.toString()}/$idPrefix/$id"
    json.getString("@type") shouldEqual s"nxv:$tpe"
    json.getString("_uuid") should fullyMatch regex """[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"""
    json.getString("label") shouldEqual label
    json.getString("name") shouldEqual name
    json.getLong("_rev") shouldEqual rev
    json.getBoolean("_deprecated") shouldEqual deprecated
  }

}
