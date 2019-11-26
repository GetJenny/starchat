import NativePackagerHelper._
import com.typesafe.sbt.packager.docker._

name := "StarChat"
organization := "com.getjenny"
maintainer := "angelo@getjenny.com"

crossScalaVersions := Seq("2.12.10")

resolvers += Resolver.bintrayRepo("hseeberger", "maven")

libraryDependencies ++= {
  val AkkaHttpVersion	= "10.1.9"
  val AkkaVersion	= "2.5.25"
  val AnalyzerVersion = "3.0.0"
  val BreezeVersion	= "0.13.2"
  val CourierVersion = "1.0.0"
  val ESClientVersion	= "7.4.2"
  val LogbackVersion	= "1.2.3"
  val ManausLibVersion = "1.0.2"
  val RoundeightsHasherVersion	= "1.2.0"
  val ScalatestVersion	= "3.0.5"
  val ScalazVersion	= "7.2.24"
  val ScoptVersion	= "3.7.0"
  val StanfordCoreNLP = "3.9.2"
  val TikaVersion	= "1.18"
  Seq(
    "com.getjenny" %% "manaus-lib" % ManausLibVersion,
    "com.getjenny" %% "analyzer" % AnalyzerVersion,
    "com.github.scopt" %% "scopt" % ScoptVersion,
    "com.roundeights" %% "hasher" % RoundeightsHasherVersion,
    "com.typesafe.akka" %% "akka-actor" % AkkaVersion,
    "com.typesafe.akka" %% "akka-contrib" % AkkaVersion,
    "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-core" % AkkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-testkit" % AkkaHttpVersion % Test,
    "com.typesafe.akka" %% "akka-stream-testkit" % AkkaVersion % Test,
    "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % Test,
    "com.typesafe.akka" %% "akka-slf4j" % AkkaVersion,
    "ch.qos.logback" % "logback-classic" % LogbackVersion,
    "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
    "org.apache.tika" % "tika-app" % TikaVersion,
    "org.apache.tika" % "tika-core" % TikaVersion,
    "org.apache.tika" % "tika-parsers" % TikaVersion,
    "edu.stanford.nlp" % "stanford-corenlp" % StanfordCoreNLP,
    "org.elasticsearch.client" % "elasticsearch-rest-client" % ESClientVersion,
    "org.elasticsearch.client" % "elasticsearch-rest-high-level-client" % ESClientVersion,
    "org.elasticsearch" % "elasticsearch" % ESClientVersion,
    "org.scalanlp" %% "breeze" % BreezeVersion,
    "org.scalanlp" %% "breeze-natives" % BreezeVersion,
    "org.scalatest" %% "scalatest" % ScalatestVersion % Test,
    "org.scalaz" %% "scalaz-core" % ScalazVersion,
    "com.github.daddykotex" %% "courier" % CourierVersion
  )
}

scalacOptions += "-deprecation"
scalacOptions += "-feature"
//scalacOptions += "-Ylog-classpath"
testOptions in Test += Tests.Argument("-oF")

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)
enablePlugins(JavaServerAppPackaging)
enablePlugins(AshScriptPlugin)
enablePlugins(UniversalPlugin)
enablePlugins(DockerPlugin)
enablePlugins(DockerComposePlugin)

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoOptions += BuildInfoOption.ToJson,
    buildInfoPackage := "com.getjenny.starchat.autogen.utils"
  )

git.useGitDescribe := true

//http://www.scala-sbt.org/sbt-native-packager/formats/docker.html
dockerCommands := Seq(
  Cmd("FROM", "openjdk:8-jre-alpine"),
  Cmd("RUN", "apk", "update"),
  Cmd("RUN", "apk", "add", "bash"),
  Cmd("RUN", "apk", "add", "curl"),
  Cmd("RUN", "addgroup", "-S", "starchat", "&&", "adduser", "-S", "starchat", "-G", "starchat"),
  Cmd("USER", "starchat:starchat"),
  Cmd("LABEL", "maintainer=\"Angelo Leto <angelo@getjenny.com>\""),
  Cmd("LABEL", "description=\"Docker container for StarChat\""),
  Cmd("WORKDIR", "/"),
  Cmd("ADD", "--chown=starchat:starchat", "/opt/docker", "/starchat"),
  Cmd("VOLUME", "/starchat/config"),
  Cmd("VOLUME", "/starchat/log")
)

packageName in Docker := packageName.value
version in Docker := version.value
dockerRepository := Some("getjenny")

//dockerImageCreationTask := (publishLocal in Docker).value
composeNoBuild := true
composeFile := "docker-starchat/docker-compose.test.yml"

// Assembly settings
mainClass in Compile := Some("com.getjenny.starchat.Main")

fork in Test := true
javaOptions in Test ++= Seq("-Dconfig.file=./src/test/resources/application.conf")

// do not buffer test output
logBuffered in Test := false

mappings in Universal ++= {
  // copy configuration files to config directory
  directory("scripts") ++
    contentOf("src/main/resources").toMap.mapValues("config/" + _).toSeq
}

scriptClasspath := Seq("../config/") ++ scriptClasspath.value

licenses := Seq(("GPLv2", url("https://www.gnu.org/licenses/old-licenses/gpl-2.0.md")))

