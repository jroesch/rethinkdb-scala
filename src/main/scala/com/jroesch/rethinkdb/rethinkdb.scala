package com.jroesch

import com.rethinkdb.{ QL2 => Protocol }
import com.jroesch.rethinkdb.json._
import com.jroesch.rethinkdb.rexp._
import scala.collection.JavaConversions._
import scala.language.implicitConversions

package object rethinkdb extends ReQLExp.DSL {
  def error(msg: String) = throw new Exception(msg)

  /** Value level implicit conversions for the DSL */
  implicit def noneToJSON(n: Option[Nothing]) = JSONNull
  implicit def boolToJSON(b: Boolean): JSONBool = JSONBool(b)
  implicit def stringToJSON(s: String): JSONString = JSONString(s)
  implicit def intToJSON(i: Int): JSONNumber = JSONNumber(i)
  implicit def doubleToJSON(d: Double): JSONNumber = JSONNumber(d)

  /** Function for building Objects from pairs. */
  implicit def obj(kvs: (String, JSON)*) = JSONObject(kvs.toMap)

  /** Function for building Array from values. */
  implicit def array(values: JSON*) = JSONArray(values.toArray)

  implicit class JSONOps(val value: JSON) extends AnyVal {
    def fromJSON(implicit json: FromJSON[Protocol.Datum]): Protocol.Datum = json.parseJSON(value)
  }

  /** Exposes needed implicits for using the API locally. Allowing REPL style interaction with RethinkDB from the
    * Scala console.
    */
  object local {
    implicit val conn = new Connection("localhost", 28015, "test")
  }

  def dbCreate(name: String) = new Document {
    val term: Protocol.Term = Term(Protocol.Term.TermType.DB_CREATE, None, Datum(name) :: Nil)
  }

  def dbDrop(name: String) = new Document {
    val term = Term(Protocol.Term.TermType.DB_DROP, Some(Datum(name)))
  }

  def dbList = new Document {
    val term = Term(Protocol.Term.TermType.DB_LIST, None)
  }

  def table(name: String, opt: JSONObject = null): Table = new Table {
    val term = Term(Protocol.Term.TermType.TABLE, None, Datum(name) :: Nil)
  }

  def db(name: String): Database = new Database {
    val term = Database(name)
  }

  /* r.count, r.sum shortcuts */
  def count = ???
  def sum = ???
  def avg = ???

  object expr extends ReQLPoly
}
