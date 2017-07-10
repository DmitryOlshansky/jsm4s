name         := "jsm4s"

organization := "olshansky.me"

version      := "0.3.0"

scalaVersion := "2.12.2"

scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.0.3",
  "com.github.tototoshi" %% "scala-csv" % "1.3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.6.0",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)
