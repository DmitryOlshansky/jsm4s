name         := "jsm4s"

organization := "olshansky.me"

version      := "1.2.0"

scalaVersion := "2.12.2"

scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.0.3",
  "com.github.tototoshi" %% "scala-csv" % "1.3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)


assemblyJarName in assembly := "jsm4s-" + version.value + ".jar"
