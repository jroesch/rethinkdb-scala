package com.jroesch.rethinkdb

import json._
import com.rethinkdb.{ QL2 => Protocol }
import rexp._

abstract class Table extends Query with Sequence {
  def indexCreate(index: String) = term match {
    case outerQ =>
      new Document {
        val term = Term(Protocol.Term.TermType.INDEX_CREATE, None, List(outerQ, Datum(index)))
      }
  }

  def indexDrop(index: JSONString) = term match {
    case outerQ =>
      new Document {
        val term = Term(Protocol.Term.TermType.INDEX_DROP, None, List(outerQ, Datum(index)))
      }
  }

  def indexList = term match {
    case outerQ =>
      new Document {
        val term = Term(Protocol.Term.TermType.INDEX_LIST, None, outerQ :: Nil)
      }
  }

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

  /* TODO: Options need some helps */
  def get(name: String) = term match {
    case outerQ => new Document {
      val term = Term(Protocol.Term.TermType.GET, None, outerQ :: Nil)
    }
  }

  def getAll(name: String, index: Option[String] = None) = term match {
    case outerQ => new Document {
      val term = Term(Protocol.Term.TermType.GET_ALL, None, outerQ :: Nil)
    }
  }

  def between(lowerKey: JSON, upperKey: JSON, index: Option[String] = None) = term match {
    case outerQ => new Document {
      val term = null //Term(Protocol.Term.TermType.BETWEEN, )
    }
  }

  def filter = ???

  def innerJoin(
      otherSeq: Sequence,
      pred: (ReQLExp[RObject], ReQLExp[RObject]) => ReQLExp[RBool]): Sequence = ???

  def outerJoin = ???
  def eqJoin    = ???
}
