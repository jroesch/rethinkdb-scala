package com.jroesch.rethinkdb

import com.rethinkdb.{ QL2 => Protocol }
import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.language.postfixOps
import json._

abstract class Query extends QueryBuilder {
  //type Result = R
  protected val database: Option[String] = None
  protected val query: Protocol.Term

  def run[A](implicit conn: Connection)/* (implicit evidence: T =:= A) */ = {
    /* val db = database match {
      case Some(dbname) => Database(dbname)
      case None => Database(conn.db)
    } */
    conn writeQuery Query(query, conn.obtainToken(), Map() + db)
    val response = Protocol.Response.parseFrom(conn.readResponse())
    response.getType match {
      case Protocol.Response.ResponseType.SUCCESS_ATOM =>
        response.getResponseList8.toList match {
          case x :: Nil => x.toJSON
          case xs  => JSONArray(xs map { _.toJSON } toArray)
        }
      case _ => response
    }
  }
}

trait QueryBuilder {
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
  
