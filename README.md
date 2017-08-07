# jsm4s

JSM is a machine learning algorithm based on mining of associative rules by induction followed by filtering via various validation logic procedures. The different flavors of JSM found in the wild is most of the time due to different sets of logic procedures implemented. Find more about JSM in the [introductory blog post](https://olshansky.me/jsm/ml/scala/2017/08/05/jsm-as-machine-learning.html).

jsm4s strives to provide the most complete and fast implementations of the following in Scala:
- mining step, also usable as FCA concepts mining or frequent closed itemset mining
- various JSM logic procedure that filters what miner would produce
- rule based classifer that works on model including potentially hand-crafted hypotheses
- reading/writing of popular data formats (CSV and FIMI for now)

Future goal is to add support for Apache Spark and better integration with other ML toolkits.

## Examples

Run any of the `scripts/*.sh`.

A sample ML session against Mushroom data set is as follows.
First encode the dataset to FIMI format using first attribute (0-based indexing) as target property:
```
java -jar jsm4s-1.4.0.jar encode -p 0 data/mushroom.csv mushroom.dat
```

Then split into training and validation datasets
```
java -jar jsm4s-1.4.0.jar split 8:2 mushroom.dat training.dat verify.dat
```

Make a test dataset with hidden property values, also known as tau examples in JSM parlance:
```
java -jar jsm4s-1.4.0.jar tau verify.dat tau.dat
```

Train a JSM model, specifying number of binary properties:
```
java -jar jsm4s-1.4.0.jar generate -m model.dat training.dat
```

Run classifier (prediction in JSM parlance):
```
java -jar jsm4s-1.4.0.jar predict -m model.dat -o predictions.dat tau.dat
```

Finally estimate correctness:
```
java -jar jsm4s-1.4.0.jar stats verify.dat predictions.dat
```

Which should produce something close to the following:
```
14:45:15.591 [INFO] Correct predictions ratio 1594/1594
14:45:15.593 [INFO] Unknown ratio 0/1594
14:45:15.593 [INFO] Conflicts ratio 0/1594
14:45:15.593 [INFO] Stats calculation took 0.182 seconds
```
The number of examples may vary because split command is using random sampling.
