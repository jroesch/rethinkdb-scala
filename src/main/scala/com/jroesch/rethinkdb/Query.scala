package com.jroesch.rethinkdb

import com.rethinkdb.QL2._
import errors.NotYetImplemented

/* im pretty sure there is a better place to put these implicits, look at 
 * scalaz */

object Query {
  trait JSON[A] // { def toJson(x: A): String; def fromJson(x: String): A } 

  implicit object IntJSON extends JSON[Int]

  implicit object DoubleJSON extends JSON[Double]

  implicit object StringJSON extends JSON[String]

  implicit object ArrayJSON extends JSON[Array[_]] //I'm fucked here

  implicit object MapJSON extends JSON[Map[_, _]]
}

trait Query[T] {
  import Query.JSON

  def run(implicit conn: Connection) = ???
  //def insert(implicit ev: T =:= Table)
  //

  def buildGlobalOptArgs(conn: Connection) = {
    val pair = Query.AssocPair.newBuilder()
    pair.setKey("db")
    val term = Term.newBuilder()
    term.setType(Term.TermType.DB)
    val termArgs = Term.newBuilder()
    termArgs.setType(Term.TermType.DATUM)
    termArgs.setDatum(datnum(conn.db))
    term.setArgs(termArgs.build)
    pair.setValue(term.build)
    pair.build
  }  

  def datum[A: JSON](d: A): Datum = {
    val datum = Datum.newBuilder()
    d match {
      case null =>
        datum.setType(Datum.DatumType.R_NULL)
      case b: Boolean =>
        datum.setType(Datum.DatumType.R_BOOL)
      case i: Int =>
        datum.setType(Datum.DatumType.R_NUM)
        datum.setRNum(i)
      case d: Double =>
        datum.setType(Datum.DatumType.R_NUM)
        datum.setRNum(d)
      case s: String =>
        datum.setType(Datum.DatumType.R_STR)
        datum.setRStr(s)
      case a: Array[_] => ???
      case m: Map[_,_]   => ???
    }

    datum.build
  }
}
  
