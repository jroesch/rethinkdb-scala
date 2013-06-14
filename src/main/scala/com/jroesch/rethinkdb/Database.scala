package com.jroesch.rethinkdb

import com.rethinkdb.{ QL2 => Protocol }
/* Phantom Type for typing Queries */
abstract class Database extends Query {
  /* Sub-Queries on a Database */
  def table(name: String) = term match {
    case outerQ => new Table {
      val term = Term(Protocol.Term.TermType.TABLE, None, List(outerQ, Datum(name)))
    }
  }

  def tableCreate(tableName: String,
                  primaryKey: Option[String] = None,
                  hardDurability: Option[Boolean] = None,
                  cacheSize: Option[Int] = None,
                  datacenter: Option[String] = None) = term match {
    case outerQ => new Table {
      val term = Term(Protocol.Term.TermType.TABLE_CREATE, None, List(outerQ, Datum(tableName)))
    }
  }

  def tableDrop = term match {
    case outerQ => new Document {
      val term = Term(Protocol.Term.TermType.TABLE_DROP, None, outerQ :: Nil)
    }
  }

  def tableList = term match {
    case outerQ => new Sequence {
      val term = Term(Protocol.Term.TermType.TABLE_LIST, None, outerQ :: Nil)
    }
  }
}
