package com.getjenny.starchat.analyzer.analyzers

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 03/03/17.
  */

import com.getjenny.analyzer.analyzers._
import com.getjenny.analyzer.operators._
import com.getjenny.starchat.analyzer.atoms._

class StarChatAnalyzer(command: String, restrictedArgs: Map[String, String])
  extends {
    override val atomicFactory = new StarchatFactoryAtomic
    override val operatorFactory = new DefaultFactoryOperator
  } with DefaultParser(command: String, restrictedArgs: Map[String, String])
