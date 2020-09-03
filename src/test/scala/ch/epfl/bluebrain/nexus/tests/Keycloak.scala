package ch.epfl.bluebrain.nexus.tests

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import ch.epfl.bluebrain.nexus.commons.http.HttpClient.UntypedHttpClient
import io.circe.Json
import io.circe.optics.JsonPath._
import monix.bio.Task
import scala.concurrent.duration._

trait Keycloak

object Keycloak extends Keycloak {

  import monix.execution.Scheduler.Implicits.global

  private val realmEndpoint =
    s"http://${System.getProperty("keycloak:8080")}/auth/realms/nexus-tests/protocol/openid-connect/token"

  private val _access_token = root.access_token.string

  def userToken(user: User, client: Client)(implicit cl: UntypedHttpClient[Task],
                                            um: FromEntityUnmarshaller[Json],
                                            materializer: Materializer): String = {
    val response = cl(
      HttpRequest(
        method = POST,
        uri = realmEndpoint,
        entity = akka.http.scaladsl.model.FormData(
          Map(
            "username"     -> user.name,
            "password"     -> user.password,
            "client_id"     -> client.id,
            "client_secret" -> client.secret,
            "grant_type"    -> "password"
          )
        ).toEntity
      )
    ).flatMap { res =>
      Task.deferFuture{ um(res.entity) }
    }.onErrorRestartLoop((10, 20.second)) { (err, state, retry) =>
           val (maxRetries, delay) = state
      if (maxRetries > 0)
            retry((maxRetries - 1, delay * 2)).delayExecution(delay)
      else
        Task.raiseError(err)
    }.runSyncUnsafe()
    _access_token.getOption(response)
      .getOrElse(
        throw new IllegalArgumentException(
          s"Couldn't get a token for user ${user.name}, we got response: $response"
        )
      )
  }


  def serviceAccountToken(client: Client)(implicit cl: UntypedHttpClient[Task],
                                          um: FromEntityUnmarshaller[Json],
                                          materializer: Materializer): String = {
    val response = cl(
      HttpRequest(
        method = POST,
        uri = realmEndpoint,
        headers = Authorization(HttpCredentials.createBasicHttpCredentials(client.id, client.secret)) :: Nil,
        entity = akka.http.scaladsl.model.FormData(
          Map(
            "grant_type"    -> "client_credentials"
          )
        ).toEntity
      )
    ).flatMap { res =>
      Task.deferFuture{ um(res.entity) }
    }.runSyncUnsafe()

    _access_token.getOption(response)
      .getOrElse(
        throw new IllegalArgumentException(
          s"Couldn't get a token for client ${client.id}, we got response: $response"
        )
      )
  }
}
