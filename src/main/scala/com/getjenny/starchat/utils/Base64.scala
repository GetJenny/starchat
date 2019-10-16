package com.getjenny.starchat.utils

object Base64 {
  def decode(in: String): String = {
    val decodedBytes = java.util.Base64.getDecoder.decode(in)
    new String(decodedBytes, "UTF-8")
  }

  def encode(in: String): String = {
    val encodedBytes = java.util.Base64.getEncoder.encode(in.getBytes)
    new String(encodedBytes, "UTF-8")
  }
}
