package com.getjenny.starchat.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 03/04/17.
  */

object TokenizersDescription {
  val analyzersMap: Map[String, (String, String)] = Map[String, (String, String)](
    "raw" -> ("jenny_raw_analyzer", "lowercase"),
    "base" -> ("jenny_base_analyzer", "lowercase"),
    "space_punctuation" -> ("jenny_space_punctuation_analyzer", "lowercase"),
    "stop" -> ("jenny_stop_analyzer", "lowercase + stopwords elimination"),
    "base_stem" -> ("jenny_base_stem_analyzer", "lowercase + stemming"),
    "stop_stem" -> ("jenny_stem_analyzer", "lowercase + stopwords elimination + stemming"),
    "shingles2" -> ("jenny_shingles_2_analyzer", "2-grams"),
    "shingles3" -> ("jenny_shingles_3_analyzer", "3-grams"),
    "shingles4" -> ("jenny_shingles_4_analyzer", "4-grams"),
    "shingles2_10" -> ("jenny_shingles_2_10_analyzer", "from 2 to 10 n-grams"),
    "ngram2" -> ("jenny_ngram_2_analyzer", "character level n-grams"),
    "stemmed_ngram2" -> ("jenny_stemmed_ngram_2_analyzer", "character level n-grams"),
    "ngram3" -> ("jenny_ngram_3_analyzer", "character level n-grams"),
    "stemmed_ngram3" -> ("jenny_stemmed_ngram_3_analyzer", "character level n-grams"),
    "ngram4" -> ("jenny_ngram_4_analyzer", "character level n-grams"),
    "stemmed_ngram4" -> ("jenny_stemmed_ngram_4_analyzer", "character level n-grams")
  )
}

case class TokenizerQueryRequest(
  tokenizer: String,
  text: String
)

case class TokenizerResponseItem(
                                  startOffset: Int,
                                  position: Int,
                                  endOffset: Int,
                                  token: String,
                                  tokenType: String
)

case class TokenizerResponse(
  tokens: List[TokenizerResponseItem] = List.empty[TokenizerResponseItem]
)
