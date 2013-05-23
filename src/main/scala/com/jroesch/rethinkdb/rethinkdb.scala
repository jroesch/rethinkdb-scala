package com.jroesch

import com.rethinkdb.{ QL2 => P }
import com.jroesch.rethinkdb.Query._

package object rethinkdb {
  /* extend the traits here */
  /* TODO: Top Level API here */

  /* JSON Trait */
  trait JSON[A]

  implicit object IntJSON extends JSON[Int]

  implicit object DoubleJSON extends JSON[Double]

  implicit object StringJSON extends JSON[String]

  implicit object ArrayJSON extends JSON[Array[_]] //I'm fucked here

  implicit object MapJSON extends JSON[Map[_, _]]

  val testConn = new Connection("localhost", 28015, "test")

  def dbCreate(name: String) = new Query[Database] {
    val query: P.Term = Term(P.Term.TermType.DB_CREATE, Some(Datum(name)))
  }

  def dbDrop(name: String) = new Query[Database] {
    val query = Term(P.Term.TermType.DB_DROP, Some(Datum(name)))
  }

  def dbList = new Query[Database] {
      val query = Term(P.Term.TermType.DB_LIST, None)
  }
}
