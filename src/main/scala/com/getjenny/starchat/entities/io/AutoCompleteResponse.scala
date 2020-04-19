package com.getjenny.starchat.entities.io

import com.getjenny.starchat.entities.persistents.SuggestedQuery

case class AutoCompleteResponse(
                                 number: Long,
                                 maxScore: Double,
                                 suggestions: List[SuggestedQuery]
                               )
