package com.getjenny.starchat.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 26/04/19.
  */

case class DtActionResult(success: Boolean = false,
                          code: Long = 0,
                          data: Map[String, String] = Map.empty[String, String])
