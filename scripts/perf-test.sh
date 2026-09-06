#!/bin/bash
BASEDIR=$(dirname $0)/..
CMD=$BASEDIR/target/universal/stage/bin/jsm-cli
if [ $# -ne 3 ] ; then
    echo "Expected 3 args <attrs> <objs> <density>"
    exit 1
fi
sbt stage 2>&1
ATTRS=$1
OBJS=$2
DENSITY=$3
$CMD 'random' -a $ATTRS -n $OBJS -p $DENSITY 'context.fimi'
export JAVA_OPTS="-Xmx1g -Xms1g -XX:+UseZGC"
for t in `seq 2 8` ; do 
    for algo in pcbo fjcbo ; do
        echo "Running $algo with threads = $t..."
        $CMD generate -t $t -a $algo -m model.fimi context.fimi
    done
done
