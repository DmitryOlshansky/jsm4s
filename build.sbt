name         := "jsm4s"

organization := "olshansky.me"

maintainer := "dmitry@olshansky.me"

scalaVersion := "2.12.20"

scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.0.3",
  "org.eclipse.collections" % "eclipse-collections" % "13.0.0",
  "com.github.tototoshi" %% "scala-csv" % "1.3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)

enablePlugins(GitVersioning)

enablePlugins(GitBranchPrompt)

enablePlugins(JavaAppPackaging)

git.useGitDescribe := true


