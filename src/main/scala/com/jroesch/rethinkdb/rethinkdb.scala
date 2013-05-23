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
  def listDB = {
    val builder = new Database {}
    val db = builder.db("test")
    val term = builder.Term(P.Term.TermType.DB_LIST, None)
    val query = builder.Query(term, 1, Map() + db)
    testConn.writeQuery(query)
    testConn.readResponse
  }
}
