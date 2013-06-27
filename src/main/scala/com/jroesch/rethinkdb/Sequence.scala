package com.jroesch.rethinkdb

import com.jroesch.rethinkdb.json._
import com.jroesch.rethinkdb.rexp._
import com.rethinkdb.{ QL2 => Protocol }

trait Sequence extends Query { //with Selection {
  /* enable for comphrensions here */
  def map = ???
  def flatMap = concatMap
  def concatMap = ???
  def orderBy = ???
  def skip = ???
  def limit = ???

  def between(lowerKey: JSON, upperKey: JSON, index: Option[String] = None) = term match {
    case outerQ => new Document {
      val term = null //Term(Protocol.Term.TermType.BETWEEN, )
    }
  }

  def filter = ???

  def apply(index: Long) = ???
  def apply(startIndex: Long, endIndex: Long) = ???

  def union(seq: Sequence): Sequence = ???

  def pluck(attrs: Seq[String]): Sequence = ???

  def without = ???

  /* Aggregation */
  def reduce(/* reduce function*/) = ???

  def count: Double = ???

  def distinct(seq: Sequence): Sequence = ???

  def groupedMapReduce = ???

  def groupBy = ???

  def contains = ???

  /* Joins */
  type JoinF = (ReQLExp[RObject], ReQLExp[RObject]) => ReQLExp[RBool]

  def innerJoin(other: Sequence, pred: JoinF): Sequence = {
    val function = mkFunction2(pred)
    val x = term
    val y = other.term
    new Sequence {
      val term = Term(Protocol.Term.TermType.INNER_JOIN, None, List(x, y, function))
    }
  }

  def outerJoin(other: Sequence, pred: JoinF): Sequence = {
    val function = mkFunction2(pred)
    val x = term
    val y = other.term
    new Sequence {
      val term =  Term(Protocol.Term.TermType.OUTER_JOIN, None, List(x, y, function))
    }
  }
  def eqJoin(leftAttr: String, other: Sequence, index: Option[String] = None): Sequence = {
    val args = index match {
      case None    => List(term, term, other.term)
      case Some(i) => List(term, term, other.term, DatumTerm(i))
    }
    new Sequence {
      val term = Term(Protocol.Term.TermType.EQ_JOIN, None, args)
    }
  }
}
