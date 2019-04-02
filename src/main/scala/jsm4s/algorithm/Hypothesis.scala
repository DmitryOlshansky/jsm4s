package jsm4s.algorithm

import java.util.concurrent.atomic.AtomicInteger

import jsm4s.ds.FcaSet
import jsm4s.property.Property

case class Hypothesis(intent: FcaSet, props: Property, wrong: AtomicInteger = new AtomicInteger(0), correct: AtomicInteger = new AtomicInteger(0))
