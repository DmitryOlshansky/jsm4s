#!/bin/bash
JAR=target/scala-2.12/jsm4s-0.9.0.jar

JARDATE=x`stat -c %Y $JAR`
MARKDATE=x`stat -c %Y .release.mark`

if [ $JARDATE != $MARKDATE ] ; then 
	sbt assembly
	touch $JAR .release.mark
fi

java -jar $JAR encode -p 0 data/mushroom.csv mushroom.dat
java -jar $JAR split 8:2 mushroom.dat training.dat verify.dat
java -jar $JAR tau -p1 verify.dat tau.dat
java -jar $JAR generate -p1 -m model.dat training.dat
java -jar $JAR recognize -p1 -m model.dat -o predictions.dat tau.dat
java -jar $JAR stats -p1 verify.dat predictions.dat
