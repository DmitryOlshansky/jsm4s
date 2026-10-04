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

// Keep the existing application and its native-packager launchers unchanged.
lazy val root = project.in(file("."))

lazy val spark = project.in(file("spark")).dependsOn(root).settings(
  name := "jsm4s-spark",
  scalaVersion := "2.12.20",
  libraryDependencies ++= Seq(
    "org.apache.spark" %% "spark-core" % "3.5.9" % Provided,
    "org.scalatest" %% "scalatest" % "3.0.3" % Test
  ),
  assembly / mainClass := Some("jsm4s.spark.SparkGenerateSplit"),
  assembly / assemblyJarName := "jsm4s-spark-assembly.jar",
  assembly / assemblyOption := (assembly / assemblyOption).value.withIncludeScala(false),
  // The cluster provides the logging API/backend; the local CLI keeps Logback.
  assembly / assemblyExcludedJars := (assembly / fullClasspath).value.filter { entry =>
    val name = entry.data.getName
    name.startsWith("logback-") || name.startsWith("slf4j-api-")
  },
  assembly / assemblyMergeStrategy := {
    case "logback.xml" => MergeStrategy.discard
    // An assembly is an unnamed module; dependency module descriptors conflict.
    case "module-info.class" => MergeStrategy.discard
    case path if path.startsWith("META-INF/versions/") && path.endsWith("/module-info.class") =>
      MergeStrategy.discard
    case path => (assembly / assemblyMergeStrategy).value(path)
  },
  Test / fork := true,
  Test / parallelExecution := false,
  Test / javaOptions ++= Seq(
    "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
    "--add-opens=java.base/java.nio=ALL-UNNAMED",
    "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
    "-Dspark.ui.enabled=false",
    "-Dspark.driver.host=127.0.0.1",
    "-Dspark.driver.bindAddress=127.0.0.1"
  )
)
