package com.getjenny.starchat.entities.io

/**
 *
 * The Json which is sent to the external service by the plugin atom
 *
 * @param query
 * @param parameters
 */
case class PluginInputBody (query: String, parameters: Map[String, String])
