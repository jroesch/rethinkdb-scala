package com.jroesch.rethinkdb

package errors {
  class RqlClientError(msg: String) extends Exception(msg)
  class RqlCompileError(msg: String) extends Exception(msg)
  class RqlRuntimeError(msg: String) extends Exception(msg)
}
