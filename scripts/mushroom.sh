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
java -jar $JAR encode -p 0 data/mushroom.csv mushroom.dat
echo "---"
java -jar $JAR split 8:2 mushroom.dat training.dat verify.dat
echo "---"
java -jar $JAR tau verify.dat tau.dat
echo "---"
java -jar $JAR generate -m model.dat training.dat
echo "---"
java -jar $JAR recognize -m model.dat -o predictions.dat tau.dat
echo "---"
java -jar $JAR stats verify.dat predictions.dat
