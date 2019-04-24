package com.getjenny.starchat.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
  */

import scala.collection.immutable.Map

case class ResponseRequestInUserInput(text: Option[String], img: Option[String])

case class ResponseRequestIn(conversationId: String,
                             traversedStates: Option[Vector[String]] = None,
                             userInput: Option[ResponseRequestInUserInput] = None,
                             state: Option[List[String]] = None,
                             data: Option[Map[String, String]] = None,
                             threshold: Option[Double] = None,
                             evaluationClass: Option[String] = None,
                             maxResults: Option[Int] = None,
                             searchAlgorithm: Option[SearchAlgorithm.Value] = None
                            )
