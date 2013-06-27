package com.jroesch.rethinkdb

import json._
import com.rethinkdb.{ QL2 => Protocol }
import rexp._
import com.rethinkdb.QL2.Term

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
      pred: (ReQLExp[RObject], ReQLExp[RObject]) => ReQLExp[RBool]): Sequence = {
    val function = mkFunction2(pred)
    val x = term
    val y = otherSeq.term
    new Sequence {
      val term = Term(Protocol.Term.TermType.INNER_JOIN, None, List(x, y, function))
    }
  }

  def outerJoin(
      other: Sequence,
      pred: (ReQLExp[RObject], ReQLExp[RObject]) => ReQLExp[RBool]): Sequence = {
    val function = mkFunction2(pred)
    val x = term
    val y = otherSeq.term
    new Sequence {
      val term: Term =  Term(Protocol.Term.TermType.OUTER_JOIN, None, List(x, y, function))
    }
  }
  def eqJoin(leftAttr: String, other: Sequence, index: Option[String] = None): Sequence = {
    val args = index match {
      case None    => List(term, term, other.term)
      case Some(i) => List(term, term, other.term, i)
    }
    new Sequence {
      val term = Term(Protocol.Term.TermType.EQ_JOIN, None, args)
    }
  }
}
