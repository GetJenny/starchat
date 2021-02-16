package com.getjenny.starchat.entities.io

import scalaz.Scalaz._

object Suggester extends Enumeration {
  val DEFAULT = Suggester.Value

  def value(algorithm: String): Suggester.Value = values.find(_.toString === algorithm).getOrElse(DEFAULT)
}
