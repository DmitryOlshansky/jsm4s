name         := "jsm4s"

organization := "olshansky.me"

version      := "0.2.0"

scalaVersion := "2.11.11"

scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.0.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)
