package com.jroesch.rethinkdb

import com.rethinkdb.{ QL2 => Protocol }
import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.language.postfixOps
import json._

abstract class Query extends QueryBuilder {
  //type Result = R
  //type Info = { val dbName: Option[String] }
  //protected val info: Info
  protected val query: Protocol.Term

  def run[A](implicit conn: Connection): JSON = {
    val db = conn.db; /* database match {
      case Some(dbname) => Database(dbname)
      case None => Database(conn.db)
    }*/

    val json = implicitly[ToJSON[Protocol.Datum]]
    //println(query)
    conn writeQuery Query(query, conn.obtainToken(), Map() + (db -> Database(db)))
    val response = Protocol.Response.parseFrom(conn.readResponse())
    response.getType match {
      case Protocol.Response.ResponseType.SUCCESS_ATOM =>
        response.getResponseList.toList match {
          case x :: Nil => toJSON(x)
          case xs  => JSONArray(xs map { toJSON(x) } toArray)
        }
      case _ => error("not yet supported")
    }
  }
}

trait QueryBuilder {
  type AssocPairs = Map[String,  Protocol.Term]

  /** Datum ToJSON instance. */
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
            (pair.getKey, toJSON(pair.getVal))
          }
          JSONObject(Map(pairs: _*))
      }
    }
  }

  def Database(name: String): Protocol.Term =
    Term(Protocol.Term.TermType.DB, None, Term(Protocol.Term.TermType.DATUM, Some(Datum(name))) :: Nil)

  /* Most Queries will have the type Start, good default */
  def Query(query: Protocol.Term, token: Long, globalOptArgs: AssocPairs): Protocol.Query =
    Query(Protocol.Query.QueryType.START, query, token, globalOptArgs)

  def Query(tpe: Protocol.Query.QueryType, query: Protocol.Term, token: Long, globalOptArgs: AssocPairs): Protocol.Query = {
    val _query = Protocol.Query.newBuilder()
    _query.setType(tpe)
    _query.setQuery(query)
    _query.setToken(token)
    val optargs = globalOptArgs map { mkQueryPair(_) }
    _query.addAllGlobalOptargs(asJavaIterable(optargs.toIterable))
    _query.build
  }

  def mkObject(pairs: Map[String, JSON]): Protocol.Term = {
    val term = Protocol.Term.newBuilder()
    term.setType(Protocol.Term.TermType.MAKE_OBJ)
    val termPairs = pairs.toList map { mkTermPair(_) }
    term.addAllOptargs(termPairs)
    term.build()
  }

  def mkArray(array: Array[JSON]): Protocol.Term = {
    val term = Protocol.Term.newBuilder()
    term.setType(Protocol.Term.TermType.MAKE_ARRAY)
    val termArray = array map { x => datumToTerm(Datum(x)) }
    term.addAllArgs(asJavaIterable(termArray.toIterable))
    term.build()
  }

  def mkTermPair(kv: (String, JSON)) = {
    val pair = Protocol.Term.AssocPair.newBuilder()
    pair.setKey(kv._1)
    pair.setVal(Datum(kv._2))
    pair.build()
  }

  def mkQueryPair(kv: (String, Protocol.Term)) = {
    val pair = Protocol.Query.AssocPair.newBuilder()
    pair.setKey(kv._1)
    pair.setVal(kv._2)
    pair.build()
  }

  def Term(tpe: Protocol.Term.TermType,
           datum: Option[Protocol.Datum],
           args: Seq[Protocol.Term] = Nil,
           optargs: Map[String, JSON] = Map()) = {
    val term = Protocol.Term.newBuilder()
    term.setType(tpe)
    datum foreach { case d => term setDatum d }
    term.addAllArgs(args)
    val oargs = optargs map { mkTermPair(_) }
    term.addAllOptargs(asJavaIterable(oargs))
    term.build()
  }

  implicit def datumToTerm(d: Protocol.Datum): Protocol.Term =
    Term(Protocol.Term.TermType.DATUM, Some(d))

  def Datum(json: JSON): Protocol.Datum = {
    import Protocol.Datum.DatumType
    val datum = Protocol.Datum.newBuilder()
    json match {
      case JSONNull =>
        datum.setType(DatumType.R_NULL)
      case JSONBool(b) =>
        datum.setType(DatumType.R_BOOL)
        datum.setRBool(b)
      case JSONNumber(n) =>
        datum.setType(DatumType.R_NUM)
        datum.setRNum(n)
      case JSONString(s) =>
        datum.setType(DatumType.R_STR)
        datum.setRStr(s)
      case JSONArray(array) =>
        datum.setType(DatumType.R_ARRAY)
        //datum.addAllRArray(asJavaIterable(array.map(Datum(_))))
      case JSONObject(map) => ???
      //datum.setType(DatumType.R_OBJECT)
      //datum.addAllRObject(asJavaIterable(m.asInstanceOf[Map[String, Protocol.Datum]]))
    }
    datum.build
  }
}
  
