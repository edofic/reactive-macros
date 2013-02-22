import sbt._
import Keys._

object MacroBuild extends Build{
  val buildSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.10.0",
    libraryDependencies ++= Seq(
      "org.reactivemongo" % "reactivemongo_2.10" % "0.8",
      "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
    ),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
  )

  lazy val main = Project("main", file("."), settings = buildSettings) 
}
