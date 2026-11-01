#!/usr/bin/env bash
# Upload an immutable release once; spark-submit distributes the JAR to executors.
set -euo pipefail
if [[ $# != 2 ]]; then
  echo 'Usage: bash scripts/spark-deploy.sh USER@GATEWAY RELEASE_NAME' >&2
  exit 2
fi
gateway=$1
release=$2
[[ "$gateway" =~ ^[A-Za-z0-9_][A-Za-z0-9_.@-]*$ ]] || exit 2
[[ "$release" =~ ^[A-Za-z0-9][A-Za-z0-9_-]*$ ]] || exit 2
repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
jar="$repo/spark/target/scala-2.12/jsm4s-spark-assembly.jar"
test -s "$jar"
# Parent /srv/jsm/releases must already exist and be writable.
remote="/srv/jsm/releases/$release"
ssh "$gateway" "mkdir '$remote'"
rsync -av -- "$jar" "$gateway:$remote/jsm4s-spark-assembly.jar"
rsync -av -- "$repo/scripts/spark-submit.sh" "$repo/scripts/spark-collect.sh" "$gateway:$remote/"
echo "Uploaded to $remote"
echo "On gateway: export JSM_SPARK_JAR=$remote/jsm4s-spark-assembly.jar"
