import sbt._
import Keys._
import scala.sys.process._

object RethinkDBBuild extends Build {

  lazy val rethinkdb = Project(
    id = "rethinkdb-driver",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      organization := "com.jroesch",
      version      := "0.0.1-ALPHA",
      scalaVersion := "2.10.2",

      scalacOptions := Seq(
        "-feature",
        "-deprecation",
        "-unchecked"
      ),
      
      (compile in Compile) <<= compile in Compile map { comp =>
        val protoc = "which protoc".!!
        (protoc + " ql2.proto --java_out=src/main/java/").!!
        comp
      },

      //(sourceGenerators in Compile) <+= (sourceManaged in Compile) map Protocol.gen,

      resolvers ++= Seq(
        "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
        "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
      ), 

      libraryDependencies ++= Seq(
        "com.chuusai" % "shapeless" % "2.0.0-SNAPSHOT" cross CrossVersion.full,
        "org.scalaz" %% "scalaz-core" % "7.0.0",
        "com.google.protobuf" % "protobuf-java" % "2.5.0",
        "org.specs2" %% "specs2" % "2.0" % "test"
      )
    )
  )
}

