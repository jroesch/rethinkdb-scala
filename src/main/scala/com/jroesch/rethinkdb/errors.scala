package com.jroesch.rethinkdb

object errors {
  class RqlClientError(msg: String) extends Exception(msg)
  class RqlCompileError(msg: String) extends Exception(msg)
  class RqlRuntimeError(msg: String) extends Exception(msg)
 
  def rqlClientError = ???
  def rqlCompileError = ???
  def rqlRuntimeErrro = ???
}
