package com.jroesch.rethinkdb

import com.jroesch.rethinkdb.json._
import com.jroesch.rethinkdb.rexp._
import com.rethinkdb.{ QL2 => Protocol }

trait Sequence extends Query { //with Selection {
  /* enable for comphrensions here */

  /* Selecting Data */
  def between(lowerKey: JSON, upperKey: JSON, index: Option[String] = None) = term match {
    case outerQ => new Document {
      val term = null //Term(Protocol.Term.TermType.BETWEEN, )
    }
  }

  def filter = ???

  /* Joins */
  type JoinF = (ReQLExp[RObject], ReQLExp[RObject]) => ReQLExp[RBool]

  def innerJoin(other: Sequence, pred: JoinF): Sequence = {
    val function = mkFunction2(pred)
    val x = term
    val y = other.term
    new Join {
      val term = Term(Protocol.Term.TermType.INNER_JOIN, None, List(x, y, function))
    }
  }

  def outerJoin(other: Sequence, pred: JoinF): Sequence = {
    val function = mkFunction2(pred)
    val x = term
    val y = other.term
    new Join {
      val term =  Term(Protocol.Term.TermType.OUTER_JOIN, None, List(x, y, function))
    }
  }

  def eqJoin(leftAttr: String, other: Sequence, index: Option[String] = None): Sequence = {
    val args = index match {
      case None    => List(term, term, other.term)
      case Some(i) => List(term, term, other.term, DatumTerm(i))
    }
    new Join {
      val term = Term(Protocol.Term.TermType.EQ_JOIN, None, args)
    }
  }

  /* Tranformations */
  def map(f: ReQLExp[RValue] => ReQLExp[RValue]): Sequence = term match {
    case seq =>
      val function = mkFunction1(f)
      new Sequence {
        val term = Term(Protocol.Term.TermType.MAP, None, List(seq, function))
      }
  }

  def withFields(fields: JSON*) = ???

  /* enable for comphrensions */
  def flatMap = ???
  def concatMap

  def orderBy = ???
  def skip = ???
  def limit = ???

  def apply(index: Long) = ???
  def apply(startIndex: Long, endIndex: Long) = ???

  def indexesOf = ???

}
