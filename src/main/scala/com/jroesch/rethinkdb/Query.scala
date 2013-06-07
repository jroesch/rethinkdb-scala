package com.jroesch.rethinkdb

import com.rethinkdb.{ QL2 => Protocol }
import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.language.postfixOps
import json._

/* im pretty sure there is a better place to put these implicits, look at 
 * scalaz */

//trait JSON[A] // { def toJson(x: A): String; def fromJson(x: String): A }

object Query {
  private[this] object queryBuilder extends Queries

  implicit def intToDatum(i: Int) = queryBuilder.Datum(i)
  implicit def doubleToDatum(d: Double) = queryBuilder.Datum(d)
  implicit def strToDatum(s: String) = queryBuilder.Datum(s)
  implicit def boolToDatum(b: Boolean) = queryBuilder.Datum(b)
  implicit def NoneToDatum(n: Option[Nothing]): Protocol.Datum =
    queryBuilder.Datum[Option[Nothing]](null)(implicitly[Datum[Option[Nothing]]])
}

abstract class Query extends Queries {
  //type Result = R

  protected val query: Protocol.Term

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
          JSONArray(x getRArrayList() map { toJSON(_) } toArray)
        case DatumType.R_OBJECT =>
          val pairs = x.getRObjectList.map { pair =>
            (pair.getKey, toJSON(pair.getVal))
          }
          JSONObject(Map(pairs: _*))
      }
    }
  }

  case class DatumOps(datum: Protocol.Datum) {
    def toJSON(implicit json: ToJSON[Protocol.Datum]): JSON = json.toJSON(datum)
  }

  implicit def datumToDatumOps(datum: Protocol.Datum): DatumOps = DatumOps(datum)
  implicit def arrayToJSON(array: Array[JSON]): JSON = JSONArray(array)
  implicit def mapToJSON(map: Map[String, JSON]): JSON = JSONObject(map)

  def run[A](implicit conn: Connection)/* (implicit evidence: T =:= A) */ = {
    //build query
    conn writeQuery Query(query, conn.obtainToken(), Map() + Database(conn.db))
    val response = Protocol.Response.parseFrom(conn.readResponse())
    response.getType match {
      case Protocol.Response.ResponseType.SUCCESS_ATOM =>
        response.getResponseList.toList match {
          case x :: Nil => x.toJSON
          case xs  => xs map { _.toJSON }
        }
      case _ => ???
    }
  }
}

trait Queries {
  type AssocPairs = Map[String, Protocol.Term]

  def Database(name: String)(implicit evidence: Datum[String]): (String, Protocol.Term) = {
    import Protocol.Term.TermType
    ("db", Term(TermType.DB, None, Term(TermType.DATUM, Some(Datum(name))) :: Nil))
  }

  /* Most Queries will have the type Start, good default */
  def Query(query: Protocol.Term, token: Long, globalOptArgs: AssocPairs): Protocol.Query =
    Query(Protocol.Query.QueryType.START, query, token, globalOptArgs)

  def Query(tpe: Protocol.Query.QueryType, query: Protocol.Term, token: Long, globalOptArgs: AssocPairs): Protocol.Query = {
    val _query = Protocol.Query.newBuilder()
    _query.setType(tpe)
    _query.setQuery(query)
    _query.setToken(token)
    val optargs = globalOptArgs map { case (k, v) =>
      val pair = Protocol.Query.AssocPair.newBuilder()
      pair.setVal(v)
      pair.setKey(k)
      pair.build
    }
    _query.addAllGlobalOptargs(asJavaIterable(optargs))
    _query.build
  }

  def Term(tpe: Protocol.Term.TermType, datum: Option[Protocol.Datum], args: Seq[Protocol.Term] = Nil, optargs: AssocPairs = Map()) = {
    val term = Protocol.Term.newBuilder()
    term.setType(tpe)
    datum foreach { case d => term setDatum d }
    term.addAllArgs(args)
    val oargs = optargs map { case (k, v) =>
      val pair = Protocol.Term.AssocPair.newBuilder()
      pair.setVal(v)
      pair.setKey(k)
      pair.build
    }
    term.addAllOptargs(asJavaIterable(oargs))
    term.build()
  }

  implicit def datumToTerm(d: Protocol.Datum): Protocol.Term =
    Term(Protocol.Term.TermType.DATUM, Some(d))

  def Datum[A: Datum](d: A) = {
    import Protocol.Datum.DatumType
    val datum = Protocol.Datum.newBuilder()
    d match {
      case null =>
        datum.setType(DatumType.R_NULL)
      case b: Boolean =>
        datum.setType(DatumType.R_BOOL)
      case i: Int =>
        datum.setType(DatumType.R_NUM)
        datum.setRNum(i)
      case d: Double =>
        datum.setType(DatumType.R_NUM)
        datum.setRNum(d)
      case s: String =>
        datum.setType(DatumType.R_STR)
        datum.setRStr(s)
      case a: Array[_] =>
        datum.setType(DatumType.R_ARRAY)
        datum.addAllRArray(asJavaIterable(a.asInstanceOf[Array[Protocol.Datum]]))
      case m: Map[_, _] => ???
      //datum.setType(DatumType.R_OBJECT)
      //datum.addAllRObject(asJavaIterable(m.asInstanceOf[Map[String, Protocol.Datum]]))
    }
    datum.build
  }
}
  
