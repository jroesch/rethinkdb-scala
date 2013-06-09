package com.jroesch

import com.rethinkdb.{ QL2 => Protocol }
import com.jroesch.rethinkdb.json._
import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scalaz._
import shapeless._

package object rethinkdb {
  /* extend the traits here */
  /* TODO: Top Level API here */

  /* Typeclass for Datum conversion */
  trait Datum[A]

  implicit object NoneDatum extends Datum[Option[Nothing]]
  implicit object IntDatum extends Datum[Int]
  implicit object DoubleDatum extends Datum[Double]
  implicit object BoolDatum extends Datum[Boolean]
  implicit object StringDatum extends Datum[String]
  implicit object ArrayDatum extends Datum[Array[Protocol.Datum]]
  implicit object MapDatum extends Datum[Map[String, Protocol.Datum]]

  /* for constructing Datum's using the smart constructor */
  private[this] object queryBuilder extends QueryBuilder

  implicit def intToDatum(i: Int): Protocol.Datum = queryBuilder.Datum(i)
  implicit def doubleToDatum(d: Double): Protocol.Datum = queryBuilder.Datum(d)
  implicit def strToDatum(s: String): Protocol.Datum = queryBuilder.Datum(s)
  implicit def boolToDatum(b: Boolean): Protocol.Datum = queryBuilder.Datum(b)
  implicit def NoneToDatum(n: Option[Nothing]): Protocol.Datum =
    queryBuilder.Datum[Option[Nothing]](null)(implicitly[Datum[Option[Nothing]]])

  /* JSON serialization */
  implicit object DatumToJSON extends ToJSON[Protocol.Datum] {
    def toJSON(x: Protocol.Datum): JSON = {
      import Protocol.Datum.DatumType
      x.getType match {
        case DatumType.R_NULL =>
          JSONNull
        case DatumType.R_NUM =>
          JSONNumber(x.getRNum)
        case DatumType.R_BOOL =>
          JSONBool(x.getRBool)
        case DatumType.R_STR =>
          JSONString(x.getRStr)
        case DatumType.R_ARRAY =>
          JSONArray((x getRArrayList() map { _.toJSON }).toArray)
        case DatumType.R_OBJECT =>
          val pairs = x.getRObjectList.map { pair =>
            (pair.getKey, pair.getVal.toJSON)
          }
          JSONObject(Map(pairs: _*))
      }
    }
  }

  /* JSON de-serialization */
  implicit object DatumFromJSON extends FromJSON[Protocol.Datum] {
    def parseJSON(x: JSON): Protocol.Datum = ???
  }

  implicit class DatumOps(val datum: Protocol.Datum) extends AnyVal {
    def toJSON(implicit json: ToJSON[Protocol.Datum]): JSON = json.toJSON(datum)
  }

  implicit class JSONOps(val value: JSON) extends AnyVal {
    def fromJSON(implicit json: FromJSON[Protocol.Datum]): Protocol.Datum = json.parseJSON(value)
  }

  implicit val testConn = new Connection("localhost", 28015, "test")

  def dbCreate(name: String) = new Document {
    val query: Protocol.Term = Term(Protocol.Term.TermType.DB_CREATE, None, Datum(name) :: Nil)
  }

  def dbDrop(name: String) = new Document {
    val query = Term(Protocol.Term.TermType.DB_DROP, Some(Datum(name)))
  }

  def dbList = new Document {
      val query = Term(Protocol.Term.TermType.DB_LIST, None)
  }

  def table(name: String, opt: JSONObject = null): Table = new Table {
    val query = Term(Protocol.Term.TermType.TABLE, None, Datum(name) :: Nil)
  }

  def db(name: String): Database = new Database {
    //val database = Some(name)

  }

  object toDatum extends Poly1 {
    implicit def default[T] = at[T](t => throw new Exception("can't convert to datum"))
    implicit def caseInt = at[Int](i => queryBuilder.Datum(i))
    implicit def caseDouble = at[Double](d => queryBuilder.Datum(d))
    implicit def caseBool = at[Boolean](b => queryBuilder.Datum(b))
    implicit def caseString = at[String](s => queryBuiler.Datum(s))
    //implicit def caseArray = at[Array[]]
  }
}
