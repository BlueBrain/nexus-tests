package ch.epfl.bluebrain.nexus.tests

sealed trait Identity extends Product with Serializable

final case class User(name: String, password: String) extends Identity

final case class Client(id: String, secret: String) extends Identity

object Identity {

  case object Anonymous extends Identity

  // Users
  val Alice: User = User("alice", "password")

  val Bob: User = User("bob", "password")

  def users: List[User] = Alice :: Bob :: Nil

  // Client
  val ServiceAccount: Client = Client("delta", "shhh")

}
