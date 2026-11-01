#!/usr/bin/env bash
set -euo pipefail
repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$repo"
# Run using Java 17, matching the Spark 3.5 runtime.
sbt test spark/test spark/assembly stage
echo "Spark JAR: $repo/spark/target/scala-2.12/jsm4s-spark-assembly.jar"
