package ch.epfl.bluebrain.nexus.tests

import java.util.regex.Pattern.quote

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.Uri.{Path => AkkaPath}
import akka.http.scaladsl.model.headers.{Accept, Authorization}
import akka.http.scaladsl.model.{RequestEntity, StatusCodes, HttpRequest => Req, _}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.ActorMaterializer
import akka.util.ByteString
import ch.epfl.bluebrain.nexus.commons.http.HttpClient
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.{UntypedHttpClient, _}
import ch.epfl.bluebrain.nexus.commons.http.JsonLdCirceSupport._
import ch.epfl.bluebrain.nexus.commons.test.{CirceEq, Randomness, Resources}
import ch.epfl.bluebrain.nexus.tests.config.Settings
import ch.epfl.bluebrain.nexus.tests.iam.types.{AclEntry, AclListing, User}
import com.typesafe.config.ConfigFactory
import io.circe.parser._
import io.circe.{Decoder, Json}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.reflect._

class BaseSpec
    extends WordSpecLike
    with BeforeAndAfterAll
    with Matchers
    with ScalatestRouteTest
    with ScalaFutures
    with Resources
    with Randomness
    with OptionValues
    with CirceEq {

  private[tests] val config                                 = new Settings(ConfigFactory.parseResources("test-app.conf").resolve()).appConfig
  private[tests] val credServiceAccount                     = HttpCredentials.createOAuth2BearerToken(config.iam.serviceAccountToken)
  private[tests] val credUser                               = HttpCredentials.createOAuth2BearerToken(config.iam.testUserToken)
  private[tests] val headersServiceAccount: Seq[HttpHeader] = Seq(Authorization(credServiceAccount))
  private[tests] val headersJsonUser: Seq[HttpHeader] =
    Seq(Authorization(credUser), Accept(MediaTypes.`application/json`))
  private[tests] val headersUser: Seq[HttpHeader] = Seq(Authorization(credUser))
  private[tests] val errorCtx                     = Map(quote("{error-context}") -> config.prefixes.errorContext.toString)
  private[tests] val resourceCtx                  = Map(quote("{success-context}") -> config.prefixes.coreContext.toString)
  private[tests] val resourceIamCtx               = Map(quote("{success-iam-context}") -> config.iam.coreContext.toString)
  private[tests] val adminBase                    = config.admin.uri
  private[tests] val iamBase                      = config.iam.uri
  private[tests] val kgBase                       = config.kg.uri
  private[tests] val externalStorageBase          = config.storage.external.endpoint
  private[tests] val s3Config                     = config.storage.s3
  private[tests] val replSub                      = Map(quote("{sub}") -> config.iam.testUserSub)
  private[tests] val realmLabel                   = "internal"

  override def beforeAll(): Unit = {
    super.beforeAll()
    cleanAcls
    val _ = ensureRealmExists
  }

  def cleanAcls =
    cl(Req(uri = s"$iamBase/acls/*/*?ancestors=true&self=false", headers = headersServiceAccount))
      .mapDecoded[AclListing] { (acls, result) =>
        result.status shouldEqual StatusCodes.OK

        val permissions = acls._results
          .map { acls =>
            val userAcls = acls.acl.filter {
              case AclEntry(User(`realmLabel`, config.iam.testUserSub), _) => true
              case _                                                       => false
            }
            acls.copy(acl = userAcls)
          }
          .filter(_.acl.nonEmpty)

        permissions.foreach { acl =>
          val entity =
            jsonContentOf(
              "/iam/subtract-permissions.json",
              replSub + (quote("{perms}") -> acl.acl.head.permissions.mkString("\",\""))
            ).toEntity
          cl(Req(PATCH, s"$iamBase/acls${acl._path}?rev=${acl._rev}", headersServiceAccount, entity))
            .mapResp(_.status shouldEqual StatusCodes.OK)
        }
        result.status shouldEqual StatusCodes.OK
      }

  def ensureRealmExists = {
    val body =
      jsonContentOf(
        "/iam/realms/create.json",
        Map(
          quote("{realm}") -> config.iam.testRealm
        )
      ).toEntity

    val rev = cl(Req(GET, s"$iamBase/realms/$realmLabel", headersServiceAccount, body)).jsonValue.hcursor
      .downField("_rev")
      .as[Int]
      .toOption

    rev match {
      case Some(r) =>
        cl(Req(PUT, s"$iamBase/realms/$realmLabel?rev=$r", headersServiceAccount, body))
          .mapResp(_.status shouldEqual StatusCodes.OK)
      case None =>
        cl(Req(PUT, s"$iamBase/realms/$realmLabel", headersServiceAccount, body))
          .mapResp(_.status shouldEqual StatusCodes.Created)
    }
  }

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(config.http.patienceConfig, 300 millis)

  private implicit val ec: ExecutionContextExecutor = system.dispatcher
  private implicit val mt: ActorMaterializer        = ActorMaterializer()

  private[tests] implicit val cl: UntypedHttpClient[Future] = HttpClient.untyped[Future]

  private[tests] implicit def toPath(str: String): AkkaPath = AkkaPath(str)

  private[tests] implicit class JsonSyntax(json: Json) {
    def toEntity: HttpEntity.Strict = HttpEntity(ContentTypes.`application/json`, json.noSpaces)

    def getString(field: String): String   = json.asObject.flatMap(_(field)).flatMap(_.asString).value
    def getLong(field: String): Long       = json.asObject.flatMap(_(field)).flatMap(_.asNumber).flatMap(_.toLong).value
    def getBoolean(field: String): Boolean = json.asObject.flatMap(_(field)).flatMap(_.asBoolean).value
    def getJson(field: String): Json       = json.asObject.flatMap(_(field)).value
    def getArray(field: String): Seq[Json] = json.asObject.flatMap(_(field)).flatMap(_.asArray).value

    def updateField(field: String, value: String): Json = json.mapObject(_.add(field, Json.fromString(value)))
    def removeField(field: String): Json                = removeFields(field)
    def removeFields(fields: String*): Json = json.mapObject { jsonObj =>
      fields.foldLeft(jsonObj) { (obj, field) =>
        obj.remove(field)
      }

    }
    def removeMetadata(): Json = json.removeFields("_uuid", "_createdAt", "_updatedAt")
  }

  private[tests] implicit class HttpResponseSyntax(value: Future[HttpResponse]) {

    def jsonValue(implicit um: FromEntityUnmarshaller[Json]): Json = whenReady(value)(res => um(res.entity)).futureValue

    def mapJson(body: (Json, HttpResponse) => Assertion)(implicit um: FromEntityUnmarshaller[Json]): Assertion =
      whenReady(value)(res => um(res.entity).map(json => body(json, res)).futureValue)

    def mapDecoded[A: ClassTag](body: (A, HttpResponse) => Assertion)(implicit decoder: Decoder[A]) =
      mapJson { (json, response) =>
        val obj = json
          .as[A]
          .right
          .getOrElse(throw new RuntimeException(s"Couldn't decode ${json.noSpaces} to ${classTag[A].toString()}."))
        body(obj, response)
      }

    def mapString(body: (String, HttpResponse) => Assertion)(implicit um: FromEntityUnmarshaller[String]): Assertion =
      whenReady(value)(res => um(res.entity).map(s => body(s, res)).futureValue)

    def mapByteString(
        body: (ByteString, HttpResponse) => Assertion
    )(implicit um: FromEntityUnmarshaller[ByteString]): Assertion =
      whenReady(value)(res => um(res.entity).map(s => body(s, res)).futureValue)

    def getJson[A](handler: Json => A)(implicit um: FromEntityUnmarshaller[Json]): A = {
      whenReady(value) { res =>
        um(res.entity).map(handler(_)).futureValue
      }
    }

    def mapResp(body: HttpResponse => Assertion): Assertion =
      whenReady(value) { resp =>
        resp.discardEntityBytes()
        body(resp)
      }

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

  private[tests] def projectReqJson(
      path: String = "/admin/projects/create.json",
      nxv: String = randomProjectPrefix,
      person: String = randomProjectPrefix,
      description: String = genString(),
      base: String = s"${config.admin.uri.toString()}/${genString()}",
      vocab: String = s"${config.admin.uri.toString()}/${genString()}"
  ): Json = {
    val rep = Map(
      quote("{nxv-prefix}")    -> nxv,
      quote("{person-prefix}") -> person,
      quote("{description}")   -> description,
      quote("{base}")          -> base,
      quote("{vocab}")         -> vocab
    )
    jsonContentOf(path, rep)
  }

  private[tests] def orgReqEntity(description: String = genString()): RequestEntity = {
    val rep = Map(quote("{description}") -> description)
    jsonContentOf("/admin/orgs/payload.json", rep).toEntity
  }

  def kgProjectReqEntity(path: String = "/kg/projects/project.json", name: String = genString()): RequestEntity = {
    val base = s"${config.kg.uri.toString()}/resources/$name/_/"
    val rep  = Map(quote("{name}") -> name, quote("{base}") -> base)
    jsonContentOf(path, rep).toEntity
  }

  private[tests] def createRespJson(
      id: String,
      rev: Long,
      tpe: String = "projects",
      `@type`: String = "Project",
      deprecated: Boolean = false
  ): Json = {
    val resp = resourceCtx ++ Map(
      quote("{id}")         -> id,
      quote("{type}")       -> tpe,
      quote("{@type}")      -> `@type`,
      quote("{rev}")        -> rev.toString,
      quote("{iamBase}")    -> config.iam.uri.toString(),
      quote("{realm}")      -> config.iam.testRealm,
      quote("{user}")       -> config.iam.testUserSub,
      quote("{adminBase}")  -> config.admin.uri.toString(),
      quote("{orgId}")      -> id,
      quote("{deprecated}") -> deprecated.toString()
    )
    jsonContentOf("/admin/response.json", resp)
  }

  private[tests] def validateAdminResource(
      json: Json,
      tpe: String,
      idPrefix: String,
      id: String,
      description: String,
      rev: Long,
      label: String,
      deprecated: Boolean = false
  ) = {
    json.getString("@id") shouldEqual s"${config.admin.uri.toString()}/$idPrefix/$id"
    json.getString("@type") shouldEqual tpe
    json.getString("_uuid") should fullyMatch regex """[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"""
    json.getString("_label") shouldEqual label
    json.getString("description") shouldEqual description
    json.getLong("_rev") shouldEqual rev
    json.getBoolean("_deprecated") shouldEqual deprecated
  }

}
