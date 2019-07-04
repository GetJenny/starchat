package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs

import java.util.Properties

import com.typesafe.config.ConfigFactory

object Config {
  protected val config = ConfigFactory.load().getConfig("scalajs.support")

  val libCache = config.getString("libCache")

  // read the generated version data
  protected val versionProps = new Properties()
  versionProps.load(getClass.getResourceAsStream("/version.properties"))

  val scalaVersion       = versionProps.getProperty("scalaVersion")
  val scalaMainVersion   = scalaVersion.split('.').take(2).mkString(".")
  val scalaJSVersion     = versionProps.getProperty("scalaJSVersion")
  val scalaJSMainVersion = scalaJSVersion.split('.').take(2).mkString(".")

}
