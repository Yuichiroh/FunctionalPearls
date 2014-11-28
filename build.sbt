name := "functional-pearls"

version := "0.0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" %  "1.0.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
    "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
    //"com.typesafe.akka" %% "akka-remote" % "2.3.6",
    //"com.typesafe.akka" %% "akka-actor" % "2.3.6"
)
