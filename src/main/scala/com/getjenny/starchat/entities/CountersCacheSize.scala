package com.getjenny.starchat.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 13/06/18.
  */

case class CountersCacheSize (
                               dictSizeCacheMaxSize: Option[Int] = Some(1000),
                               totalTermsCacheMaxSize: Option[Int] = Some(1000),
                               countTermCacheMaxSize: Option[Int] = Some(10000)
                             )
