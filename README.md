# jsm4s

JSM is a machine learning algorithm based on mining of associative rule sets followed by filtering via various validation logic procedures. The different flavors of JSM found in the wild is most of the time due to different sets of logic procedures implemented.

jsm4s strives to provide the most complete and fast implementations of the following in Scala:
- mining step, also known  as FCA concepts mining or frequent closed itemset mining
- JSM logic procedure that filters what the miner produced
- simple rule based classifer that works on refined hypotheses including potentially hand-crafted ones
- reading/writing of popular data formats


Potential future goals - add support for Apache Spark and/or better integration with other ML toolkits.

