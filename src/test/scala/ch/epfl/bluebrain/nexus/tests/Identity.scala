package ch.epfl.bluebrain.nexus.tests

sealed trait Identity extends Product with Serializable



object Identity {

  case object Anonymous extends Identity

  final case class UserCredentials(name: String, password: String) extends Identity

  final case class ClientCredentials(id: String, name :String, secret: String) extends Identity

  object ClientCredentials{
    def apply(id: String, secret: String): ClientCredentials =
      new ClientCredentials(id, s"service-account-$id", secret)
  }

  // Users
  val Alice: UserCredentials = UserCredentials("alice", "password")

  val Bob: UserCredentials = UserCredentials("bob", "password")

  def users: List[UserCredentials] = Alice :: Bob :: Nil

  // Client
  val ServiceAccount: ClientCredentials = ClientCredentials("delta", "shhh")

}
