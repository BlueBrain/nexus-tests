resolvers += Resolver.bintrayRepo("bbp", "nexus-releases")

addSbtPlugin("ch.epfl.bluebrain.nexus" % "sbt-nexus"          % "0.14.0")
addSbtPlugin("com.tapad"               % "sbt-docker-compose" % "1.0.35")
