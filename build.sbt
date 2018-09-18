/*
scalafmt: {
  style = defaultWithAlign
  maxColumn = 150
  align.tokens = [
    { code = "=>", owner = "Case" }
    { code = "?", owner = "Case" }
    { code = "extends", owner = "Defn.(Class|Trait|Object)" }
    { code = "//", owner = ".*" }
    { code = "{", owner = "Template" }
    { code = "}", owner = "Template" }
    { code = ":=", owner = "Term.ApplyInfix" }
    { code = "++=", owner = "Term.ApplyInfix" }
    { code = "+=", owner = "Term.ApplyInfix" }
    { code = "%", owner = "Term.ApplyInfix" }
    { code = "%%", owner = "Term.ApplyInfix" }
    { code = "%%%", owner = "Term.ApplyInfix" }
    { code = "->", owner = "Term.ApplyInfix" }
    { code = "?", owner = "Term.ApplyInfix" }
    { code = "<-", owner = "Enumerator.Generator" }
    { code = "?", owner = "Enumerator.Generator" }
    { code = "=", owner = "(Enumerator.Val|Defn.(Va(l|r)|Def|Type))" }
  ]
}
 */

// Dependency versions
val akkaVersion       = "2.5.16"
val akkaHttpVersion   = "10.1.5"
val catsVersion       = "1.3.1"
val circeVersion      = "0.9.3"
val mockitoVersion    = "2.16.0"
val pureconfigVersion = "0.9.2"
val scalaTestVersion  = "3.0.5"

// Nexus dependency versions
val commonsVersion = "0.10.28"

// Dependency modules
lazy val akkaHttpCore    = "com.typesafe.akka"     %% "akka-http-core"    % akkaHttpVersion
lazy val akkaHttpTestKit = "com.typesafe.akka"     %% "akka-http-testkit" % akkaHttpVersion
lazy val akkaStream      = "com.typesafe.akka"     %% "akka-stream"       % akkaVersion
lazy val akkaTestKit     = "com.typesafe.akka"     %% "akka-testkit"      % akkaVersion
lazy val pureconfig      = "com.github.pureconfig" %% "pureconfig"        % pureconfigVersion
lazy val scalaTest       = "org.scalatest"         %% "scalatest"         % scalaTestVersion
lazy val slf4j           = "com.typesafe.akka"     %% "akka-slf4j"        % akkaVersion

// Nexus dependency modules
lazy val commonsHttp = "ch.epfl.bluebrain.nexus" %% "commons-http" % commonsVersion
lazy val commonsTest = "ch.epfl.bluebrain.nexus" %% "commons-test" % commonsVersion

lazy val root = project
  .in(file("."))
  .settings(noPublish)
  .settings(
    name                  := "tests",
    moduleName            := "tests",
    coverageFailOnMinimum := false,
    libraryDependencies ++= Seq(
      akkaHttpCore,
      akkaStream,
      pureconfig,
      akkaHttpTestKit % Test,
      commonsHttp     % Test,
      commonsTest     % Test,
      scalaTest       % Test,
      slf4j           % Test
    ),
    parallelExecution in Test := false,
    Test / testOptions        += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-u", "target/test-reports")
  )

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

lazy val noPublish = Seq(
  publishLocal    := {},
  publish         := {},
  publishArtifact := false,
)

inThisBuild(
  List(
    homepage := Some(url("https://github.com/BlueBrain/nexus-tests")),
    licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    scmInfo  := Some(ScmInfo(url("https://github.com/BlueBrain/nexus-tests"), "scm:git:git@github.com:BlueBrain/nexus-tests.git")),
    developers := List(
      Developer("bogdanromanx", "Bogdan Roman", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("hygt", "Henry Genet", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("umbreak", "Didac Montero Mendez", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("wwajerowicz", "Wojtek Wajerowicz", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
    ),
    // These are the sbt-release-early settings to configure
    releaseEarlyWith              := BintrayPublisher,
    releaseEarlyNoGpg             := true,
    releaseEarlyEnableSyncToMaven := false,
  ))

addCommandAlias("review", ";clean;scalafmtCheck;scalafmtSbtCheck;test:scalafmtCheck;test")
