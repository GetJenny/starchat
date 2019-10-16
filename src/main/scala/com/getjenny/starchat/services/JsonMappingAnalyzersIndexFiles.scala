package com.getjenny.starchat.services

case class JsonMappingAnalyzersIndexFiles(
                           path: String,
                           updatePath: String,
                           indexSuffix: String,
                           numberOfShards: Int,
                           numberOfReplicas: Int
                         )
