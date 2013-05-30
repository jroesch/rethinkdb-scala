package com.jroesch

import com.rethinkdb.{ QL2 => Protocol }
import com.jroesch.rethinkdb.Query._
import com.jroesch.rethinkdb.json._

package object rethinkdb {
  /* extend the traits here */
  /* TODO: Top Level API here */

  /* JSON Trait */
  trait Datum[A]

  implicit object NoneDatum extends Datum[Option[Nothing]]
  implicit object IntJSON extends Datum[Int]
  implicit object DoubleJSON extends Datum[Double]
  implicit object BoolDatum extends Datum[Boolean]
  implicit object StringJSON extends Datum[String]
  implicit object ArrayJSON extends Datum[Array[_]] //I'm fucked here
  implicit object MapJSON extends Datum[Map[_, _]]

  implicit val testConn = new Connection("localhost", 28015, "test")

  def dbCreate(name: String) = new Database {
    val query: Protocol.Term = Term(Protocol.Term.TermType.DB_CREATE, Some(Datum(name)))
  }

  def dbDrop(name: String) = new Database {
    val query = Term(Protocol.Term.TermType.DB_DROP, Some(Datum(name)))
  }

  def dbList = new Database {
      val query = Term(Protocol.Term.TermType.DB_LIST, None)
  }

  def table(name: String, opt: JSONObject = null): Table = new Table {
    val query = Term(Protocol.Term.TermType.TABLE, None, Datum(name) :: Nil)
  }
}
