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

import org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings

// Dependency versions
val ammoniteVersion   = "1.7.1"
val akkaVersion       = "2.5.25"
val akkaHttpVersion   = "10.1.10"
val alpakkaVersion    = "1.1.1"
val catsVersion       = "2.0.0"
val circeVersion      = "0.12.1"
val mockitoVersion    = "2.22.0"
val pureconfigVersion = "0.12.0"
val scalaTestVersion  = "3.0.8"
val gatlingVersion    = "3.2.1"
val uuidGenVersion    = "3.2.0"
val awsSdkVersion     = "2.9.7"

// Nexus dependency versions
val commonsVersion = "0.17.11"

// Dependency modules
lazy val akkaHttpCore    = "com.typesafe.akka"      %% "akka-http-core"          % akkaHttpVersion
lazy val akkaHttpTestKit = "com.typesafe.akka"      %% "akka-http-testkit"       % akkaHttpVersion
lazy val akkaStream      = "com.typesafe.akka"      %% "akka-stream"             % akkaVersion
lazy val akkaTestKit     = "com.typesafe.akka"      %% "akka-testkit"            % akkaVersion
lazy val pureconfig      = "com.github.pureconfig"  %% "pureconfig"              % pureconfigVersion
lazy val scalaTest       = "org.scalatest"          %% "scalatest"               % scalaTestVersion
lazy val slf4j           = "com.typesafe.akka"      %% "akka-slf4j"              % akkaVersion
lazy val alpakkaSSE      = "com.lightbend.akka"     %% "akka-stream-alpakka-sse" % alpakkaVersion
lazy val uuidGen         = "com.fasterxml.uuid"     % "java-uuid-generator"      % uuidGenVersion
lazy val awsSdk          = "software.amazon.awssdk" % "s3"                       % awsSdkVersion

// Nexus dependency modules
lazy val commonsCore = "ch.epfl.bluebrain.nexus" %% "commons-core" % commonsVersion
lazy val commonsTest = "ch.epfl.bluebrain.nexus" %% "commons-test" % commonsVersion

// Performance tests
lazy val gatling       = "io.gatling"            % "gatling-test-framework"    % gatlingVersion
lazy val gatlingCharts = "io.gatling.highcharts" % "gatling-charts-highcharts" % gatlingVersion
lazy val ammoniteOps   = "com.lihaoyi"           %% "ammonite-ops"             % ammoniteVersion

lazy val root = project
  .enablePlugins(GatlingPlugin)
  .in(file("."))
  .settings(inConfig(IntegrationTest)(scalafmtConfigSettings))
  .settings(noPublish)
  .settings(
    name                     := "tests",
    moduleName               := "tests",
    coverageFailOnMinimum    := false,
    javaSpecificationVersion := "1.8",
    libraryDependencies ++= Seq(
      akkaHttpCore,
      akkaStream,
      pureconfig,
      commonsCore,
      ammoniteOps,
      akkaHttpTestKit % Test,
      awsSdk          % Test,
      commonsTest     % Test,
      scalaTest       % Test,
      slf4j           % Test,
      alpakkaSSE      % Test,
      uuidGen         % Test,
      gatling         % "test,it",
      gatlingCharts   % "test,it"
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
  publishArtifact := false
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
      Developer("wwajerowicz", "Wojtek Wajerowicz", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/"))
    ),
    // These are the sbt-release-early settings to configure
    releaseEarlyWith              := BintrayPublisher,
    releaseEarlyNoGpg             := true,
    releaseEarlyEnableSyncToMaven := false
  )
)

addCommandAlias("review", ";clean;scalafmtCheck;scalafmtSbtCheck;test:scalafmtCheck;test")
