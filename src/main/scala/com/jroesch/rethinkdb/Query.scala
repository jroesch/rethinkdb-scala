package com.jroesch.rethinkdb

import com.rethinkdb.{ QL2 => Protocol }
import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.language.postfixOps
import com.jroesch.rethinkdb.json._
import com.jroesch.rethinkdb.rexp._
import scala.reflect.runtime.universe._

abstract class Query extends QueryBuilder {
  //type Result = R
  //type Info = { val dbName: Option[String] }
  //protected val info: Info
  protected[rethinkdb] val term: Protocol.Term

  def run[A](implicit conn: Connection): JSON = {
    val db = conn.db; /* database match {
      case Some(dbname) => Database(dbname)
      case None => Database(conn.db)
    }*/

    //println(query)
    conn writeQuery Query(term, conn.obtainToken(), Map() + (db -> Database(db)))
    val response = Protocol.Response.parseFrom(conn.readResponse())
    response.getType match {
      case Protocol.Response.ResponseType.SUCCESS_ATOM =>
        response.getResponseList.toList match {
          case x :: Nil => DatumToJSON.toJSON(x)
          case xs  => JSONArray(xs map { DatumToJSON.toJSON(_) } toArray)
        }
      case Protocol.Response.ResponseType.SUCCESS_SEQUENCE =>
        val results = response.getResponseList map { DatumToJSON.toJSON(_) }
        JSONArray(results.toArray)
      case Protocol.Response.ResponseType.CLIENT_ERROR =>
        val errorMsg = DatumToJSON.toJSON(response.getResponseList.toList.head).asInstanceOf[JSONString]
        throw new errors.RqlCompileError(errorMsg.s)
      case Protocol.Response.ResponseType.COMPILE_ERROR =>
        val errorMsg = DatumToJSON.toJSON(response.getResponseList.toList.head).asInstanceOf[JSONString]
        throw new errors.RqlCompileError(errorMsg.s)
      case Protocol.Response.ResponseType.RUNTIME_ERROR =>
        val errorMsg = DatumToJSON.toJSON(response.getResponseList.toList.head).asInstanceOf[JSONString]
        throw new errors.RqlRuntimeError(errorMsg.s)
      case _ => { println(response); error("not yet supported") }
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
          JSONArray((x getRArrayList() map { toJSON(_) }).toArray)
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

  def mkFunction(arity: Int, body: ReQLExp[_]) = {
    val paramNumbers = new Array[JSON](arity)
    for (i <- 1 to arity) { paramNumbers(i - 1) = i }
    val params = mkArray(paramNumbers)
    Term(Protocol.Term.TermType.FUNC, None, List(params, body.term))
  }

  def mkFunction1 = ???

  def mkFunction2[A <: RValue, B <: RValue, C <: RValue](f: (ReQLExp[A], ReQLExp[B]) => ReQLExp[C])
                /* (implicit aTag: TypeTag[A], bTag: TypeTag[B], cTag: TypeTag[C]) */ = {
    val x = new ReQLExp[A](mkVar(1))
    val y = new ReQLExp[B](mkVar(2))
    mkFunction(2, f(x, y))
  }

  def mkVar(number: JSONNumber) =
    Term(Protocol.Term.TermType.VAR, None, DatumTerm(number) :: Nil)

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
    val datum = kv._2 match {
      case JSONObject(map) => mkObject(map)
      case JSONArray(seq)  => mkArray(seq)
      case other           => DatumTerm(other)
    }
    pair.setVal(datum)
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

  def DatumTerm(json: JSON) = datumToTerm(Datum(json))

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
  
