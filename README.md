# jsm4s

JSM is a machine learning algorithm based on mining of associative rule sets followed by filtering via various validation logic procedures. The different flavors of JSM found in the wild are most of the time due to different sets of logic procedures implemented.

jsm4s strives to provide the most complete and fast implementations of the following in Scala:
- mining step, also usable as FCA concepts mining or frequent closed itemset mining
- various JSM logic procedure that filters what miner would produce
- rule based classifer that works on model including potentially hand-crafted hypotheses
- reading/writing of popular data formats (CSV and FIMI for now)

Potential future goals - add support for Apache Spark and/or better integration with other ML toolkits.

## Examples

A sample ML session against Mushroom data set is as follows.
First encode the dataset to FIMI format using first attribute (0-based indexing) as target property:
```
java -jar jsm4s-1.0.0.jar encode -p 0 data/mushroom.csv mushroom.dat
```

Then split into training and validation datasets
```
java -jar jsm4s-1.0.0.jar split 8:2 mushroom.dat training.dat verify.dat
```

Make a test dataset with hidden property values, also known as tau examples in JSM parlance:
```
java -jar jsm4s-1.0.0.jar tau verify.dat tau.dat
```

Train a JSM model, specifying number of binary properties:
```
java -jar jsm4s-1.0.0.jar generate -m model.dat training.dat
```

Run classifier (prediction in JSM parlance):
```
java -jar jsm4s-1.0.0.jar recognize -m model.dat -o predictions.dat tau.dat
```

Finally estimate correctness:
```
java -jar jsm4s-1.0.0.jar stats verify.dat predictions.dat
```

Which should produce something close to the following:
```
Correct predictions ratio 1607/1607
Unknown ratio 0/1607
Conflicts ratio 0/1607
[main] INFO jsm4s.EntryPoint$ - Time: 0.24817 sec
```
The number of examples may vary because split command is using random sampling.
