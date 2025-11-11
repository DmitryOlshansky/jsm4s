name         := "jsm4s"

organization := "olshansky.me"

scalaVersion := "2.13.16"

scalacOptions := Seq("-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "5.2.0",
  "com.github.tototoshi" %% "scala-csv" % "2.0.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
  "org.slf4j" % "slf4j-api" % "1.7.36",
  "ch.qos.logback" % "logback-classic" % "1.5.21",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)

//enablePlugins(GitVersioning)

//enablePlugins(GitBranchPrompt)

//enablePlugins(JavaAppPackaging)

//git.useGitDescribe := true


