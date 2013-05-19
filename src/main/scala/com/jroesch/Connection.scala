package com.jroesch.rethinkdb

import java.io._
import java.net._
import com.rethinkdb.VersionDummy

class Connection(address: String, port: Int) {
  val socket = new Socket(address, port)
  val out = socket.getOutputStream
  val in = socket.getInputStream

  def writeBytes(bytes: Array[Byte]) = out.write(bytes)

  //def reconnect() = {
  //VersionDummy.V0_1 
}
