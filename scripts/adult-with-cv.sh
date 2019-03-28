#!/bin/bash
TEMP=`mktemp`
tail -F $TEMP &
JAR=`sbt assembly 2>&1 | tee $TEMP | grep -oP "(?<=Packaging\s|Assembly up to date:\s)([/A-Za-z0-9.-]+)"`
kill $!

if [ "x$JAR" == "x" ] ; then
	echo "Failed to assemble the JAR"
	exit 1
fi

echo "---"
java -jar $JAR encode -p 14 data/adult.csv adult.dat
echo "---"
java -jar $JAR split 8:2 adult.dat training-cv.dat verify.dat
echo "---"
java -jar $JAR split 8:2 training-cv.dat training.dat cv.dat
echo "--- Produce TAU set from verify data"
java -jar $JAR tau verify.dat tau.dat
echo "--- Generate model"
java -jar $JAR generate -m model.dat training.dat
echo "--- Refine model via cross-validation"
java -jar $JAR refine -m model.dat -o cv-model.dat cv.dat
echo "--- Prediction on original model"
java -jar $JAR predict -m model.dat -o predictions.dat tau.dat
echo "--- Calculate basic stats on prediction results"
java -jar $JAR stats verify.dat predictions.dat
echo "--- Prediction on refined model"
java -jar $JAR predict -m cv-model.dat -o cv-predictions.dat tau.dat
echo "--- Calculate basic stats on refined model"
java -jar $JAR stats verify.dat cv-predictions.dat