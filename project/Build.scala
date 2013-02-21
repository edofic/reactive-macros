import sbt._
import Keys._

object MacroBuild extends Build{
  val buildSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.10.0",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
  )

  lazy val main = Project("main", file("."), settings = buildSettings) 
}
