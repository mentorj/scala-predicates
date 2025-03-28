package com.javaxpert.scala

import org.scalatest.flatspec.AnyFlatSpec
import Predicates._
class PredicatesTestSuite extends AnyFlatSpec{
  "Negate a predicate" should "give opposite values" in {
    val pred1 = Predicate(x=>false)
    val negatePred = pred1.negate()
    val res1= pred1.test(5)
    val expected = true
    val negateValue= negatePred.test(5)
    assert(expected==negateValue)
    assert(negateValue != res1)
  }

  "Chaining predicates with and" should "apply both predicates and compose the resuult with AND operator" in{
    val pred1 = Predicate(condFn= (x:Int) => x%2==0)
    val pred2 = Predicate(condFn= (x:Int) => x>3)
    val composedPred = pred1.and(pred2)
    val res1 = pred1.test(6)
    val res2 = pred2.test(6)
    val composedRes = composedPred.test(6)
    assert(composedRes==res2 && res1)
  }

  "Chaining predicates with or" should "apply both predicates and compose the resuult with OR operator" in {
    val pred1 = Predicate(condFn = (x: Int) => x % 2 == 0)
    val pred2 = Predicate(condFn = (x: Int) => x < 3)
    val composedPred = pred1.or(pred2)
    val res1 = pred1.test(6)
    val res2 = pred2.test(6)
    val composedRes = composedPred.test(6)
    assert(composedRes == res2 || res1)
  }

  "anyOf " should "return true as soon as one of the predicate matches" in {
    val pred1 = Predicate(condFn = (x: Int) => x % 2 == 0)
    val pred2 = Predicate(condFn = (x: Int) => x < 3)
    val anyOfRes = anyOf(List(pred1,pred2))(6)
    assert(anyOfRes)
  }

  "allOf " should "return true as soon as all of the predicate match" in {
    val pred1 = Predicate(condFn = (x: Int) => x % 2 == 0)
    val pred2 = Predicate(condFn = (x: Int) => x < 3)
    val allOfRes = allOf(List(pred1, pred2))(6)
    assert(!allOfRes)
  }

  "noneOf " should "return true as soon as all of the predicate do not match ( evaluate to false)" in {
    val pred1 = Predicate(condFn = (x: Int) => x % 2 == 0)
    val pred2 = Predicate(condFn = (x: Int) => x < 3)
    val noneOfRes = noneOf(List(pred1, pred2))(5)
    assert(noneOfRes)
  }

  "isIn" should "create a new Predicate returning true when value contained in the parameter values list" in{
    val refList = List(1,2,3,4,5)
    val isInPred = isIn(refList)
    val res1 = isInPred.test(2)
    assert(res1)
    val res2 = isInPred.test(8)
    assert(!res2)
  }

  "is" should "creates a new Predicate returning true if predicate application  leads to value" in{
    val isPred=is(5)
    assert(isPred.test(5))

    val isPred2 = is(true)
    assert(!isPred2.test(false))
  }
}
