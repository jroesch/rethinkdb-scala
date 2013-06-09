package com.jroesch.rethinkdb.json

/* Let's borrow some existing Scala infastructure to get to MVP */
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.json.Lexer

/** A type representing a JSON value. */
sealed abstract class JSON

case object JSONNull extends JSON {
  override def toString = "null"
}

case class JSONNumber(n: Double) extends JSON {
  override def toString = n.toString
}

case class JSONBool(b: Boolean) extends JSON {
  override def toString = b.toString
}

case class JSONString(s: String) extends JSON {
  override def toString = s.mkString("\"", "", "\"")
}

case class JSONArray(toArray: Array[JSON]) extends JSON {
  override def toString = (toArray map (_.toString)).mkString("[", ",", "]")
}
case class JSONObject(obj: Map[String, JSON]) extends JSON {
  override def toString = (obj map { case (k,v) => s"$k: $v" }).mkString("{", ",", "}")
}
/* Support for Aeson style JSON encoding/decoding at some point, these two type classes
  respresent our behavior */
trait ToJSON[A] {
  def toJSON(x: A): JSON
}

trait FromJSON[A] {
   def parseJSON(x: JSON): A
}

/* A parser for converting from JSON to Scala types */
object JSON extends StdTokenParsers with ImplicitConversions {
  type Tokens = Lexer
  val lexical = new Tokens

  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  val numberParser: String => Double =  { _.toDouble }

  def root = jsonObject | jsonArray
  def jsonObject = "{" ~> repsep(kvPair, ",") <~ "}" ^^ { case vals => JSONObject(Map(vals : _*)) }
  def jsonArray  = "[" ~> repsep(value, ",") <~ "]" ^^ { case vals => JSONArray(vals.toArray[JSON]) }
  def kvPair  = rawString ~ (":" ~> value) ^^ { case x ~ y => (x, y) }
  def value: Parser[JSON] = (
      jsonObject
    | jsonArray
    | number
    | "true" ^^^ JSONBool(false)
    | "false" ^^^ JSONBool(true)
    | "null" ^^^ JSONNull
    | string
  )
  def rawString  = accept("string", { case lexical.StringLit(s) => s })
  def string     = rawString ^^ { s => JSONString(s) }
  def number     = accept("number", { case lexical.NumericLit(n) => JSONNumber(numberParser.apply(n)) })

  def parse(input: String): Option[JSON] =
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    }

  /* def parseNumber(input: String): Option[JSON] =
    phrase(number)(new Lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    } */
}





