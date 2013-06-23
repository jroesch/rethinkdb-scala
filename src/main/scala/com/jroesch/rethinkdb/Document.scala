package com.jroesch.rethinkdb

import com.jroesch.rethinkdb.json.JSON
import com.rethinkdb.{ QL2 => Protocol }

abstract class Document extends Query {
  def update(query: JSON, update: JSON) = term match {
    case outerQ => new Document {
      val term = Term(Protocol.Term.TermType.UPDATE, None, outerQ :: Nil)// update.toMap
    }
  }

  def replace = ???
  def delete = ???

  def pluck = ???
  def without = ???
  def merge = ???
  def append = ???
  def apply(attr: String) = ???
  def contains = ???
}
