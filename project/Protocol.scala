import sbt._
import scala.io.Source._

object Protocol {
  def gen(dir: File) = {
    val ql2 = dir / ".." / "src" / "main" / "java" / "com" / "rethinkdb" / "QL2.java" 
    IO.write(ql2, fromFile("ql2.proto").mkString)
    Seq(ql2)
  }
}
