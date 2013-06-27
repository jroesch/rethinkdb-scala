package com.jroesch.rethinkdb

import com.rethinkdb.{ QL2 => Protocol }
import com.jroesch.rethinkdb.json._
import shapeless._
import scala.language.implicitConversions

object rexp {
  /** Phantom types for our expression language. */
  trait RValue
  trait RObject extends RValue
  trait RArray extends RValue
  trait RBool extends RValue
  trait RString extends RValue
  trait RNumber extends RValue

  /* Use a Polymorphic function value to build ReQL expressions. */
  object ReQLExp extends ReQLPoly {
    /** Enable the use of Scala values in ReQL expressions. */
    trait DSL {
      implicit def intToReQLExp(i: Int): ReQLExp[RNumber] =
        ReQLExp(i)(caseInt)
      implicit def doubleToReQLExp(d: Double): ReQLExp[RNumber] =
        ReQLExp(d)(caseDouble)
      implicit def stringToReQLExp(s: String): ReQLExp[RString] =
        ReQLExp(s)(caseString)
      implicit def arrayToReQLExp(arr: Array[JSON]): ReQLExp[RArray] =
        ReQLExp(arr)(caseArray)
      implicit def arrayToReQLExp(arr: JSONArray): ReQLExp[RArray] =
        ReQLExp(arr)(caseJSONArray)
    }
  }

  trait ReQLPoly extends Poly1 with QueryBuilder {

    /* null values ? */

    /* Booleans */
    implicit def caseBoolean = at[Boolean](b => new ReQLExp[RBool](DatumTerm(b)))
    //implicit def caseJSONBool = at[JSONBool](b => ???)

    /* Numbers */
    implicit def caseInt =
      at[Int](i => new ReQLExp[RNumber](DatumTerm(i)))
    implicit def caseDouble =
      at[Double](d => new ReQLExp[RNumber](DatumTerm(d)))
    /* implicit def caseJSONNumber = at[JSONNumber](x => x match {
      case JSONNumber(d) =>
        new ReQLExp[RNumber](qb.DatumTerm(d))
    })*/

    /* Strings */
    implicit def caseString =
      at[String](s => new ReQLExp[RString](DatumTerm(s)))

    implicit def caseJSONString = at[JSONString](s => ???)

    /* Array */
    implicit def caseArray =
      at[Array[JSON]](arr => new ReQLExp[RArray](mkArray(arr)))
    implicit def caseJSONArray = at[JSONArray](arr => arr match {
      case JSONArray(seq) => new ReQLExp[RArray](mkArray(seq))
    })

    /* Object */
    implicit def caseJSONObject = at[JSONObject](obj => ???)
  }

  /** A expression language for building first class functions for ReQL queries. */
  class ReQLExp[+A <: RValue](val term: Protocol.Term) extends Query {

    def apply(key: ReQLExp[RString]): ReQLExp[RValue] = {
      new ReQLExp[RValue](Term(Protocol.Term.TermType.GETATTR, None, term :: key.term :: Nil))
    }

    def +(x: ReQLExp[RValue]): ReQLExp[RValue] = {
      new ReQLExp[RValue](Term(Protocol.Term.TermType.ADD, None, term :: x.term :: Nil))
    }

    def -(x: ReQLExp[RValue]): ReQLExp[RValue] = {
      new ReQLExp[RValue](Term(Protocol.Term.TermType.SUB, None, term :: x.term :: Nil))
    }

    def *(x: ReQLExp[RValue]): ReQLExp[RValue] = {
      new ReQLExp[RValue](Term(Protocol.Term.TermType.MUL, None, term :: x.term :: Nil))
    }

    /* Hopefully constraint these more later */
    def /(x: ReQLExp[RValue]): ReQLExp[RNumber] = ???
    def %(x: ReQLExp[RValue]): ReQLExp[RNumber] = ???
    def &(x: ReQLExp[RValue]): ReQLExp[RBool] = ???
    def |(x: ReQLExp[RValue]): ReQLExp[RBool] = ???

    def ==[B <: RValue](x: ReQLExp[B]): ReQLExp[RBool] =
      new ReQLExp[RBool](Term(Protocol.Term.TermType.EQ, None, term :: x.term :: Nil))

    def !=[B <: RValue](x: ReQLExp[B]): ReQLExp[RBool] =
      new ReQLExp[RBool](Term(Protocol.Term.TermType.NE, None, term :: x.term :: Nil))

    def >[B <: RValue](x: ReQLExp[B]): ReQLExp[RBool] =
      new ReQLExp[RBool](Term(Protocol.Term.TermType.GT, None, term :: x.term :: Nil))

    def >=[B <: RValue](x: ReQLExp[B]): ReQLExp[RBool] =
      new ReQLExp[RBool](Term(Protocol.Term.TermType.GE, None, term :: x.term :: Nil))

    def <[B <: RValue](x: ReQLExp[B]): ReQLExp[RBool] =
      new ReQLExp[RBool](Term(Protocol.Term.TermType.LT, None, term :: x.term :: Nil))

    def <=[B <: RValue](x: ReQLExp[B]): ReQLExp[RBool] =
      new ReQLExp[RBool](Term(Protocol.Term.TermType.LE, None, term :: x.term :: Nil))

    def unary_~(implicit ev: A <:< RBool): ReQLExp[RBool] = ???

    def smatch(regex: String): ReQLExp[RValue] = ???
  }
}


