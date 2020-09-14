package ch.epfl.bluebrain.nexus.tests.kg

import ch.epfl.bluebrain.nexus.tests.Identity.UserCredentials
import ch.epfl.bluebrain.nexus.tests.{Identity, NewBaseSpec, Realm}
import monix.execution.Scheduler.Implicits.global

class NewResourceSpec extends NewBaseSpec {

  private val testRealm   = Realm("archives" + genString())
  private val testClient = Identity.ClientCredentials(genString(), genString(), testRealm)
  private val Rick = UserCredentials(genString(), genString(), testRealm)
  private val Morty = UserCredentials(genString(), genString(), testRealm)

  private val orgId   = genId()
  private val projId1 = genId()
  private val projId2 = genId()
  private val id1     = s"$orgId/$projId1"
  private val id2     = s"$orgId/$projId2"
  println(id1 + id2)

  override def beforeAll(): Unit = {
    super.beforeAll()
    initRealm(
      testRealm,
      Identity.ServiceAccount,
      testClient,
      Rick :: Morty :: Nil
    ).runSyncUnsafe()
  }

}
