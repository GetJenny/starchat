package com.getjenny.starchat.entities.io

/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 12/04/2020.
  */


case class AutoCompleteRequest(userText: String,
                               suggesterType: Option[Suggester.Value] = Some(Suggester.DEFAULT),
                               sortAlgorithm: Option[SuggesterSortAlgorithm.Value] = Some(SuggesterSortAlgorithm.DEFAULT),
                               threshold: Option[Double] = None,
                               suggestionCategories: Option[List[String]] = None,
                               maxResults: Option[Int] = None,
                              )
