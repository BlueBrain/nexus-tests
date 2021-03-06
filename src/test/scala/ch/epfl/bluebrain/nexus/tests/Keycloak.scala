package ch.epfl.bluebrain.nexus.tests

import java.util.regex.Pattern.quote

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import ch.epfl.bluebrain.nexus.commons.test.Resources
import ch.epfl.bluebrain.nexus.tests.Identity.{ClientCredentials, UserCredentials}
import ch.epfl.bluebrain.nexus.tests.Optics._
import com.typesafe.scalalogging.Logger
import io.circe.Json
import monix.bio.Task

import scala.concurrent.duration._

object Keycloak extends Resources {

  import monix.execution.Scheduler.Implicits.global

  private val logger = Logger[this.type]

  private val keycloakUrl      = s"http://${System.getProperty("keycloak:8080")}/auth"
  private val keycloakAdminUrl = s"$keycloakUrl/admin/realms"
  // Defined in docker-compose file
  private val adminRealm     = Realm("master")
  private val keycloakAdmin  = UserCredentials("admin", "admin", adminRealm)
  private val keycloakClient = ClientCredentials("admin-cli", "", adminRealm)

  def importRealm(realm: Realm, clientCredentials: ClientCredentials, userCredentials: List[UserCredentials])(
      implicit cl: UntypedHttpClient[Task],
      um: FromEntityUnmarshaller[Json],
      materializer: Materializer
  ): Task[StatusCode] = {
    logger.info(s"Creating realm $realm in Keycloak...")
    val users = userCredentials.zipWithIndex.flatMap {
      case (u, i) =>
        Map(
          quote(s"{user$i}")          -> u.name,
          quote(s"{user_password$i}") -> u.password
        )
    }.toMap

    val json = jsonContentOf(
      "/iam/keycloak/import.json",
      Map(
        quote("{realm}")         -> realm.name,
        quote("{client}")        -> clientCredentials.id,
        quote("{client_secret}") -> clientCredentials.secret
      ) ++ users
    )

    for {
      adminToken <- userToken(keycloakAdmin, keycloakClient)
      status <- cl(
        HttpRequest(
          method = POST,
          uri = keycloakAdminUrl,
          headers = Authorization(HttpCredentials.createOAuth2BearerToken(adminToken)) :: Nil,
          entity = HttpEntity(ContentTypes.`application/json`, json.noSpaces)
        )
      ).tapError { t =>
          Task { logger.error(s"Error while importing realm: $realm", t) }
        }
        .map { res =>
          res.status
        }
    } yield status
  }

  private def realmEndpoint(realm: Realm) =
    Uri(s"$keycloakUrl/realms/${realm.name}/protocol/openid-connect/token")

  def userToken(user: UserCredentials, client: ClientCredentials)(
      implicit cl: UntypedHttpClient[Task],
      um: FromEntityUnmarshaller[Json],
      materializer: Materializer
  ): Task[String] = {
    logger.info(s"Getting token for user ${user.name} for ${user.realm}")
    val clientFields = if (client.secret == "") {
      Map("client_id" -> client.id)
    } else {
      Map(
        "client_id"     -> client.id,
        "client_secret" -> client.secret
      )
    }

    val request = HttpRequest(
      method = POST,
      uri = realmEndpoint(user.realm),
      entity = akka.http.scaladsl.model
        .FormData(
          Map(
            "username"   -> user.name,
            "password"   -> user.password,
            "grant_type" -> "password"
          ) ++ clientFields
        )
        .toEntity
    )

    cl(request)
      .flatMap { res =>
        Task.deferFuture { um(res.entity) }
      }
      .onErrorRestartLoop((10, 10.seconds)) { (err, state, retry) =>
        // We have a retry here as the first thing we do with keycloak is getting a token
        // And without a warmup, we can get an UnexpectedConnectionClosureException
        // because Keycloak is still starting
        val (maxRetries, delay) = state
        if (maxRetries > 0)
          retry((maxRetries - 1, delay)).delayExecution(delay)
        else
          Task.raiseError(err)
      }
      .tapError { t =>
        Task { logger.error(s"Error while getting user token for realm: ${user.realm} and user:$user", t) }
      }
      .map { response =>
        keycloak.access_token
          .getOption(response)
          .getOrElse(
            throw new IllegalArgumentException(
              s"Couldn't get a token for user ${user.name}, we got response: $response"
            )
          )
      }

  }

  def serviceAccountToken(client: ClientCredentials)(
      implicit cl: UntypedHttpClient[Task],
      um: FromEntityUnmarshaller[Json],
      materializer: Materializer
  ): Task[String] = {
    logger.info(s"Getting token for client ${client.name} for ${client.realm}")
    cl(
      HttpRequest(
        method = POST,
        uri = realmEndpoint(client.realm),
        headers = Authorization(HttpCredentials.createBasicHttpCredentials(client.id, client.secret)) :: Nil,
        entity = akka.http.scaladsl.model
          .FormData(
            Map(
              "grant_type" -> "client_credentials"
            )
          )
          .toEntity
      )
    ).flatMap { res =>
        Task.deferFuture { um(res.entity) }
      }
      .tapError { t =>
        Task { logger.error(s"Error while getting user token for realm: ${client.realm} and client: $client", t) }
      }
      .map { response =>
        keycloak.access_token
          .getOption(response)
          .getOrElse(
            throw new IllegalArgumentException(
              s"Couldn't get a token for client ${client.id}, we got response: $response"
            )
          )
      }
  }
}
