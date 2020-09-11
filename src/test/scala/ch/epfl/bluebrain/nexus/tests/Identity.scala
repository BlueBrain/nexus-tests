package ch.epfl.bluebrain.nexus.tests

sealed trait Identity extends Product with Serializable

object Identity {

  case object Anonymous extends Identity

  sealed trait Authenticated extends Identity {
    def name: String

    def realm: Realm
  }

  final case class UserCredentials(name: String,
                                   password: String,
                                   realm: Realm) extends Authenticated

  final case class ClientCredentials(id: String,
                                     name :String,
                                     secret: String,
                                     realm: Realm) extends Authenticated

  object ClientCredentials{
    def apply(id: String, secret: String, realm: Realm): ClientCredentials =
      new ClientCredentials(id, s"service-account-$id", secret, realm)
  }

  import Realm._
  // Users
  val Alice: UserCredentials = UserCredentials("alice", "password", internal)

  val Bob: UserCredentials = UserCredentials("bob", "password", internal)

  def users: List[UserCredentials] = Alice :: Bob :: Nil

  // Client
  val ServiceAccount: ClientCredentials = ClientCredentials("delta", "shhh", internal)

}
