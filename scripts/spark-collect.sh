#!/usr/bin/env bash
# Local/shared POSIX output only; Hadoop/object-store URIs need their storage CLI.
set -euo pipefail
if [[ $# != 2 ]]; then
  echo 'Usage: bash scripts/spark-collect.sh OUTPUT_DIRECTORY MODEL_FILE' >&2
  exit 2
fi
output=$1
model=$2
test -f "$output/_COMPLETE"
test -f "$output/hypotheses/_SUCCESS"
test -s "$output/header.txt"
test -s "$output/metadata.json"
shopt -s nullglob
parts=("$output"/hypotheses/part-*)
(( ${#parts[@]} > 0 )) || { echo 'No output part files found' >&2; exit 1; }
# Refuse replacement. An incomplete local merge can be removed/retried by the user.
set -o noclobber
cat -- "$output/header.txt" "${parts[@]}" > "$model"
echo "Collected: $model"
