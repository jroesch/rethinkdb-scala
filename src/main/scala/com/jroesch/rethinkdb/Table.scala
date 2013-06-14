package com.jroesch.rethinkdb

import json._
import com.rethinkdb.{ QL2 => Protocol }

abstract class Table extends Query {
  def indexCreate = ???
  def indexDrop = ???
  def indexList = ???
  def insert(obj: JSON)/* (opts: JSON = ) */ = query match {
    case outerQ =>
      val insertee = obj match {
        case JSONObject(map) => mkObject(map)
        case JSONArray(array) => mkArray(array)
      }
      new Document {
        val query = Term(Protocol.Term.TermType.INSERT, None, outerQ :: insertee :: Nil)
      }
  }

  def update = ???
  def replace = ???
  def delete = ???

  def get(name: String) = query match {
    case outerQ => new Document {
      val query = Term(Protocol.Term.TermType.GET, None, outerQ :: Nil)
    }
  }

  def getAll(name: String, index: Option[String] = None) = ???
  def between(lowerKey: JSON, upperKey: JSON, index: Option[String] = None) = ???
  def filter = ???

  def innerJoin = ???
  def outerJoin = ???
  def eqJoin    = ???
}
