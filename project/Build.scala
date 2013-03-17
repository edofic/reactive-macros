import sbt._
import Keys._

object MacroBuild extends Build{
  val buildSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.10.0",
    scalacOptions ++= Seq("-feature", "-deprecation"),
    libraryDependencies ++= Seq(
      "org.reactivemongo" % "reactivemongo_2.10" % "0.8",
      "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
    ),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
  )

  lazy val main = Project("main", file("."), settings = buildSettings).settings(
    organization := "com.edofic",
    name := "reactivemacros",
    version  := "0.1-SNAPSHOT",
    publishTo := Some(Resolver.file("snapshots", new File(Path.userHome, "/git/repository/snapshots")))
  )
}
