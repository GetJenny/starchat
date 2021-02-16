package com.getjenny.starchat.entities.persistents

import com.getjenny.starchat.entities.io.SuggestionCategory

/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 19/04/20.
  */

case class SuggestedQuery(
                           score: Double = 0,
                           query: String,
                           category: SuggestionCategory.Value
                         )
