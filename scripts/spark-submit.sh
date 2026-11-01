#!/usr/bin/env bash
set -euo pipefail
if (( $# < 3 )); then
  echo 'Usage: bash scripts/spark-submit.sh INPUT_URI OUTPUT_URI SPLITS [application options]' >&2
  exit 2
fi
: "${SPARK_HOME:?Set SPARK_HOME to Spark 3.5.9 (Scala 2.12)}"
repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
jar=${JSM_SPARK_JAR:-$repo/spark/target/scala-2.12/jsm4s-spark-assembly.jar}
test -s "$jar"
input=$1
output=$2
splits=$3
shift 3
exec "$SPARK_HOME/bin/spark-submit" \
  --class jsm4s.spark.SparkGenerateSplit \
  --master "${JSM_SPARK_MASTER:-local[2]}" \
  --deploy-mode client \
  --driver-memory "${JSM_DRIVER_MEMORY:-2g}" \
  --executor-memory "${JSM_EXECUTOR_MEMORY:-2g}" \
  --executor-cores "${JSM_EXECUTOR_CORES:-1}" \
  --total-executor-cores "${JSM_TOTAL_CORES:-4}" \
  --driver-java-options \
  "-XX:StartFlightRecording=settings=profile,dumponexit=true" \
  --conf \
  "spark.executor.extraJavaOptions=-XX:StartFlightRecording=settings=profile,dumponexit=true" \
  --conf spark.speculation=false \
  --conf spark.dynamicAllocation.enabled=false \
  --conf spark.task.cpus=1 \
  --conf spark.eventLog.enabled=true \
  --conf spark.eventLog.dir=file:///tmp/jsm-spark-events \
  --conf spark.eventLog.logStageExecutorMetrics=true \
  "$jar" --input "$input" --output "$output" --splits "$splits" "$@"
