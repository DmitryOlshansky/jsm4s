#!/bin/bash
DATASET=adult
PROPERTY=14
BASEDIR=$(dirname $0)/..
CMD=$BASEDIR/target/universal/stage/bin/jsm-cli
sbt stage 2>&1
echo "--- Encoding of CSV"
$CMD encode -p $PROPERTY data/$DATASET.csv $DATASET.dat
echo "--- Randomized split of dataset"
$CMD split 8:2 $DATASET.dat $DATASET-training.dat $DATASET-verify.dat
echo "--- Produce dataset with hidden value out of verify dataset"
$CMD tau $DATASET-verify.dat $DATASET-tau.dat
echo "--- Generate model"
$CMD generate -m $DATASET-model.dat $DATASET-training.dat
echo "--- Run predictions on file with tau properties"
$CMD predict -m $DATASET-model.dat -o $DATASET-predictions.dat $DATASET-tau.dat
echo "--- Calculate basic stats on predictions"
$CMD stats $DATASET-verify.dat $DATASET-predictions.dat
