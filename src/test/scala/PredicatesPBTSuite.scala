package com.javaxpert.scala

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Predicates.allOf
import Predicates.anyOf
import Predicates.noneOf
import Predicate._

class PredicatesPBTSuite extends PBTBaseTest {
  "identity function" should "be the neutral " in {
    val id = Predicate(x => {x == x})
    forAll{( x:Int)  => id.test(x)==true}
  }

  "double negate invokations" should "return initial predicate" in {
    val isEven = Predicate( (x:Int) => {x%2 ==0})
    forAll{ (x:Int) => isEven.negate().negate().test(x)  == isEven.test(x)}
  }

  "a predicate and negate for this predicate  " should "always be false" in {
    val isEven = Predicate ( (x:Int) => (x%2==0))
    forAll{ (x:Int) => !isEven.and(isEven.negate()).test(x) }
  }

  "a predicate OR negate for this predicate  " should "always be true" in {
    val isEven = Predicate((x: Int) => (x % 2 == 0))
    forAll { (x: Int) => isEven.or(isEven.negate()).test(x) }
  }

  "using allOf with a predicate and its nagate " should "always be false" in {
    val isEven = Predicate((x: Int) => (x % 2 == 0))
    val preds:List[Predicate[Int]] = List(isEven,isEven.negate())
    forAll { (x: Int) =>
      !allOf(preds)(x)
    }
  }


  "using noneOf with a predicate and its nagate " should "always be false" in {
    val isEven = Predicate((x: Int) => (x % 2 == 0))
    val preds:List[Predicate[Int]] = List(isEven,isEven.negate())
    forAll { (x: Int) =>
      !noneOf(preds)(x)
    }
  }


  "using anyOf with a predicate and its nagate " should "always be true" in {
    val isEven = Predicate((x: Int) => (x % 2 == 0))
    val preds:List[Predicate[Int]] = List(isEven,isEven.negate())
    forAll { (x: Int) =>
      anyOf(preds)(x) 
    }
  }
}

object PredicatesPBTSuite {
}
