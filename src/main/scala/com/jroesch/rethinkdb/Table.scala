package com.jroesch.rethinkdb

import json._
import com.rethinkdb.{ QL2 => Protocol }
import rexp._

abstract class Table extends Query with Sequence {
  def indexCreate = ???
  def indexDrop = ???
  def indexList = ???
  def insert(obj: JSON)/* (opts: JSON = ) */ = term match {
    case outerQ =>
      val insertee = obj match {
        case JSONObject(map) => mkObject(map)
        case JSONArray(array) => mkArray(array)
      }
      new Document {
        val term = Term(Protocol.Term.TermType.INSERT, None, outerQ :: insertee :: Nil)
      }
  }

  def update = ???
  def replace = ???
  def delete = ???

  def get(name: String) = term match {
    case outerQ => new Document {
      val term = Term(Protocol.Term.TermType.GET, None, outerQ :: Nil)
    }
  }

  def getAll(name: String, index: Option[String] = None) = ???
  def between(lowerKey: JSON, upperKey: JSON, index: Option[String] = None) = ???
  def filter = ???

  def innerJoin(
      otherSeq: Sequence,
      pred: (ReQLExp[RObject], ReQLExp[RObject]) => ReQLExp[RBool]): Sequence = ???

  def outerJoin = ???
  def eqJoin    = ???
}
