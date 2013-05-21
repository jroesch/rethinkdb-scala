package com.jroesch.rethinkdb

import java.io._
import java.net._
import com.rethinkdb.QL2.VersionDummy
import scala.language.postfixOps

class Connection(address: String, port: Int, var db: String) {
  val socket = new Socket(address, port)
  val out = socket.getOutputStream
  val in = socket.getInputStream

  writeBytes(ibytes(VersionDummy.Version.V0_1_VALUE))

  private def ibytes(i: Int): Array[Byte] = Array (
    (i & 0x000000FF)       toByte,
    (i & 0x0000FF00) >> 8  toByte,
    (i & 0x00FF0000) >> 16 toByte,
    (i & 0xFF000000) >> 24 toByte
  )

  def writeBytes(bytes: Array[Byte]) = out.write(bytes)
  
  //def reconnect() = {
  //VersionDummy.V0_1 
}
