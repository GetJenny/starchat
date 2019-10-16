package com.getjenny.starchat.utils

import com.getjenny.starchat.entities.CommonOrSpecificSearch
import com.getjenny.starchat.services.TermService

import scala.util.matching.Regex

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 19/07/18.
  */

object Index {

  private[this] val orgNameRegex: String = "[a-z0-9\\-]{1,256}"
  private[this] val langRegex: String = "[a-z]{1,256}"
  private[this] val arbitraryPatternRegex: String = "[a-z0-9_]{1,256}"

  val orgNameMatchRegexDelimited: Regex = ("^(" + orgNameRegex + ")$").r

  /** regular expression to match index names */
  val systemIndexMatchRegex: Regex = "(starchat_system_[a-z0-9\\-]{1,256})".r
  val systemIndexMatchRegexDelimited: Regex = ("^" + systemIndexMatchRegex + "$").r

  val indexMatchRegex: Regex = ("(index_(?:" +
    orgNameRegex + ")_(?:" + langRegex + ")_(?:" + arbitraryPatternRegex + "))").r
  val indexMatchRegexDelimited: Regex = ("^" + indexMatchRegex + "$").r

  val indexExtractFieldsRegex: Regex = ("""(?:index_(""" +
    orgNameRegex + """)_(""" + langRegex + ")_(" + arbitraryPatternRegex + "))").r
  val indexExtractFieldsRegexDelimited: Regex = ("^" + indexExtractFieldsRegex + "$").r

  /** Extract language from index name
    *
    * @param indexName the full index name
    * @return a tuple with the two component of the index (org, language, arbitrary)
    */
  def patternsFromIndexName(indexName: String): (String, String, String) = {
    val (organization, language, arbitrary) = indexName match {
      case indexExtractFieldsRegexDelimited(orgPattern, languagePattern, arbitraryPattern) =>
        (orgPattern, languagePattern, arbitraryPattern)
      case _ => throw new Exception("index name is not well formed")
    }
    (organization, language, arbitrary)
  }

  /** calculate and return the full index name
    *
    * @param indexName the index name
    * @param suffix the suffix name
    * @return the full index name made of indexName and Suffix
    */
  def indexName(indexName: String, suffix: String): String = {
    if(suffix.nonEmpty) {
      indexName + "." + suffix
    } else indexName
  }

  /** Extract the name of the common index
    *
    * @param indexName the index name e.g. index_getjenny_english_0
    * @param useDefaultOrg force to use the default org instead of the organization pattern
    * @return the name of the language specific common index e.g. index_getjenny_english_common_0
    */
  def getCommonIndexName(indexName: String, useDefaultOrg: Boolean = true): String = {
    val arbitraryPattern =  TermService.commonIndexArbitraryPattern
    val (organization, language, _) = patternsFromIndexName(indexName)
    val org = if (useDefaultOrg) {
      TermService.defaultOrg
    } else {
      organization
    }
    "index_" + org + "_" + language + "_" + arbitraryPattern
  }

  /** resolve the index name using the common or specific information
    *
    * @param indexName the index name
    * @param commonOrSpecific IDXSPECIFIC or COMMON
    * @return the common index name otherwise the indexName without modifications
    */
  def resolveIndexName(indexName: String, commonOrSpecific: CommonOrSpecificSearch.Value): String = {
    commonOrSpecific match {
      case CommonOrSpecificSearch.IDXSPECIFIC => indexName
      case _ => getCommonIndexName(indexName)
    }
  }
}
