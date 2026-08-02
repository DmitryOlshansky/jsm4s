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
for algo in cbo fcbo pcbo pfcbo ; do
$CMD generate -a $algo -m model-$algo.fimi context.fimi
done
