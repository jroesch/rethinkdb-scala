package com.jroesch.rethinkdb.json

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.json.Lexer
import shapeless._
import scala.annotation.tailrec

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

trait JSONPrettyPrinter { self: JSON =>
  override def toString = {
    var indent = 0

    val builder = new StringBuilder

    //@tailrec make this true
    //this needs some work
    def pprint(x: JSON, sb: StringBuilder): Unit = x match {
      case JSONObject(toMap) =>
        sb ++= "{\n"
        indent += 1
        for ((k, v) <- toMap) {
          sb ++= "  " * indent + k.toString + ": "
          pprint(v, sb)
          sb ++= ",\n"
        }
        indent -= 1
        sb ++= "  " * indent + "}"
      case JSONArray(array) =>
        sb ++= "  " * indent + "[\n"
        indent += 1
        for (each <- array)
          pprint(each, sb)
        indent -= 1
        sb ++= "  " * indent + "\n]"
      case other => sb ++= other.toString
    }

    pprint(this, builder)
    builder.toString
  }
}
case class JSONArray(toArray: Array[JSON]) extends JSON with JSONPrettyPrinter

case class JSONObject(toMap: Map[String, JSON]) extends JSON with JSONPrettyPrinter

/** A parser for converting from JSON in string form to the JSON value type */
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

/** A type class for converting from a type A to the JSON type */
trait ToJSON[A] {
  def toJSON(x: A): JSON
}

/** A type class for converting from the JSON type to a type A */
trait FromJSON[A] {
  def parseJSON(x: JSON): A
}

object ToJSONInstances {
  implicit object NoneToJSON extends ToJSON[Option[Nothing]] {
    def toJSON(none: Option[Nothing]) = JSONNull
  }
  implicit object BoolToJSON extends ToJSON[Boolean] {
    def toJSON(bool: Boolean) = JSONBool(bool)
  }

  implicit object ByteToJSON extends ToJSON[Byte] {
    def toJSON(byte: Byte) = JSONNumber(byte)
  }

  implicit object ShortToJSON extends ToJSON[Short] {
    def toJSON(short: Short) = JSONNumber(short)
  }

  implicit object IntToJSON extends ToJSON[Int] {
    def toJSON(int: Int) = JSONNumber(int)
  }

  implicit object LongToJSON extends ToJSON[Long] {
    def toJSON(long: Long) = JSONNumber(long)
  }

  implicit object BigIntToJSON extends ToJSON[BigInt] {
    def toJSON(bigInt: BigInt) = JSONNumber(bigInt.toDouble)
  }

  implicit object FloatToJSON extends ToJSON[Float] {
    def toJSON(float: Float) = JSONNumber(float)
  }

  implicit object DoubleToJSON extends ToJSON[Double] {
    def toJSON(double: Double) = JSONNumber(double)
  }

  implicit object StringToJSON extends ToJSON[String] {
    def toJSON(str: String) = JSONString(str)
  }

  implicit def SeqToJSON[A: ToJSON] = new ToJSON[Seq[A]] {
    def toJSON(seq: Seq[A]) = {
      val A = implicitly[ToJSON[A]]
      val values = seq map { A.toJSON(_) }
      JSONArray(values.toArray)
    }
  }

  implicit object SeqJSONToJSON extends ToJSON[Seq[JSON]] {
    def toJSON(seq: Seq[JSON]) = JSONArray(seq.toArray)
  }
}