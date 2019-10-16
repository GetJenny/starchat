package com.getjenny.starchat.services.esclient

import com.getjenny.starchat.services.SystemIndexManagementService.config

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 14/11/17.
  */

object SystemIndexManagementElasticClient extends ElasticClient {
  override val indexName: String = config.getString("es.system_index_name")
  override val indexSuffix: String = ""

  // decision table changes awareness per index mechanism
  val dtReloadCheckFrequency : Int = config.getInt("es.dt_reload_check_frequency")
  val dtReloadTimestampFieldName = "timestamp"

  // alive nodes detection mechanism
  val clusterNodeAliveMaxInterval : Int = config.getInt("es.cluster_node_alive_max_interval")
  val clusterNodeAliveSignalInterval: Int = config.getInt("es.cluster_node_alive_sig_interval")
  val clusterNodeCleanDeadInterval: Int = config.getInt("es.cluster_node_clean_dead_interval")
  val clusterCleanDtLoadingRecordsInterval: Int = config.getInt("es.cluster_clean_dt_loading_records_interval")
  val authMethod: String = config.getString("starchat.auth_method")
  val autoInitializeSystemIndex: Boolean = config.getBoolean("es.auto_initialize_system_index")

  val numberOfShards: Int = config.getInt("es.system_idx_number_of_shards")
  val numberOfReplicas: Int = config.getInt("es.system_idx_number_of_replicas")
}
