import NativePackagerHelper._
import com.typesafe.sbt.packager.docker._

name := "StarChat"
organization := "com.getjenny"
maintainer := "angelo@getjenny.com"

crossScalaVersions := Seq("2.12.8")

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.bintrayRepo("hseeberger", "maven"))

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers +=
  "Sonatype OSS Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"

libraryDependencies ++= {
  val AkkaHttpVersion	= "10.1.8"
  val AkkaVersion	= "2.5.22"
  val BreezeVersion	= "0.13.2"
  val ESClientVersion	= "7.0.1"
  val LogbackVersion	= "1.2.3"
  val RoundeightsHasherVersion	= "1.2.0"
  val ScalatestVersion	= "3.0.5"
  val ScalazVersion	= "7.2.24"
  val ScoptVersion	= "3.7.0"
  val TikaVersion	= "1.18"
  val ManausLibVersion = "1.0.1"
  val StanfordCoreNLP = "3.9.2"
  val AnalyzerVersion = "1.0.11"
  val CourierVersion = "1.0.0"
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
enablePlugins(UniversalPlugin)
enablePlugins(DockerPlugin)
enablePlugins(DockerComposePlugin)

git.useGitDescribe := true

//http://www.scala-sbt.org/sbt-native-packager/formats/docker.html
dockerCommands := Seq(
  Cmd("FROM", "java:11"),
  Cmd("RUN", "apt", "update"),
  Cmd("RUN", "apt", "install", "-y", "netcat"),
  Cmd("LABEL", "maintainer=\"Angelo Leto <angelo@getjenny.com>\""),
  Cmd("LABEL", "description=\"Docker container for StarChat\""),
  Cmd("WORKDIR", "/"),
  Cmd("ADD", "/opt/docker", "/starchat"),
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

