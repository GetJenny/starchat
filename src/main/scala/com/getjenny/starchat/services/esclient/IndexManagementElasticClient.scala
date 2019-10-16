package com.getjenny.starchat.services.esclient

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 10/03/17.
  */

object IndexManagementElasticClient extends ElasticClient {
  override val indexName: String = ""
  override val indexSuffix: String = ""

  val stateNumberOfShards: Int = config.getInt("es.state_idx_number_of_shards")
  val stateNumberOfReplicas: Int = config.getInt("es.state_idx_number_of_replicas")
  val kbNumberOfShards: Int = config.getInt("es.kb_idx_number_of_shards")
  val kbNumberOfReplicas: Int = config.getInt("es.kb_idx_number_of_replicas")
  val logsDataNumberOfShards: Int = config.getInt("es.logs_data_idx_number_of_shards")
  val logsDataNumberOfReplicas: Int = config.getInt("es.logs_data_idx_number_of_replicas")
  val priorDataNumberOfShards: Int = config.getInt("es.prior_data_idx_number_of_shards")
  val priorDataNumberOfReplicas: Int = config.getInt("es.prior_data_idx_number_of_replicas")
  val termNumberOfShards: Int = config.getInt("es.term_idx_number_of_shards")
  val termNumberOfReplicas: Int = config.getInt("es.term_idx_number_of_shards")
}
