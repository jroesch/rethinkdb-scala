package com.jroesch.rethinkdb

import java.net._
import com.rethinkdb.QL2.VersionDummy
import com.rethinkdb.{ QL2 => Protocol }
import scala.language.postfixOps
import scala.language.implicitConversions

class Connection(address: String, port: Int, var db: String) {
  val socket = new Socket(address, port)
  val out = socket.getOutputStream
  val in = socket.getInputStream

  writeBytes(VersionDummy.Version.V0_1_VALUE)

  private implicit def intToBytes(i: Int): Array[Byte] = Array (
    (i & 0x000000FF)       toByte,
    (i & 0x0000FF00) >> 8  toByte,
    (i & 0x00FF0000) >> 16 toByte,
    (i & 0xFF000000) >> 24 toByte
  )

  private implicit def bytesToInt(a: Array[Byte]): Int =
    (a(3) << 24) + (a(2) << 16) + (a(1) << 8) + a(0)

  def writeBytes(bytes: Array[Byte]) { out.write(bytes) }

  def writeQuery(query: Protocol.Query) {
    val bytes = query.toByteArray
    writeBytes(bytes.length)
    writeBytes(bytes)
  }

  def readResponse() = {
    val size = new Array[Byte](4)
    in.read(size, 0, 4)
    val result = new Array[Byte](size)
    in.read(result, 0, size)
    result
  }
  
  //def reconnect() = {
  //VersionDummy.V0_1 
}
