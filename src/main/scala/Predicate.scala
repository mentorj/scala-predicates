package com.javaxpert.scala

/**
 * a predicate wraps a fonction taking any kind of parameter and returning a boolean.
 * Main function  is the test(value) checking if condition is verified for given value.
 * Prredicates can be composed with and / or to create new predicates.
 * @param condFn, function returning a Boolean
 * @tparam A, whatever type you want
 */
case class Predicate[A](condFn: A => Boolean){
  /**
   * does the value verifies the condition?
   * @param value, value to check
   * @return true if value verifies the condition,false otherwise
   */
  def test(value:A):Boolean =
    condFn(value)

  /**
   * creates a new predicate chaining 2 predicates (current and p) using AND operator
   * {{{
   *   val p1=Predicate(condFn = (x: Int) => x % 2 == 0)
   *   val p2 =Predicate(condFn = (x:Int) => x>4)
   *   val composedPred =p1.and(p2)
   *   compposedPred.test(6) // true
   * }}}
   * @param p, other predicate
   * @return a new predicate built on top of the 2 predicates
   */
  def and(p:Predicate[A]):Predicate[A] =
    Predicate(condFn= (a:A) => {condFn.apply(a) && p.condFn.apply(a)})


  /**
   * creates a new predicate chaining 2 predicates (current and p) using OR operator
   * {{{
   *   val p1=Predicate(condFn = (x: Int) => x % 2 == 0)
   *   val p2 =Predicate(condFn = (x:Int) => x<4)
   *   val composedPred =p1.or(p2)
   *    compposedPred.test(6) // true
   *
   * }}}
 *
   * @param p, other predicate
   * @return a new predicate built on top of the 2 predicates
   */
  def or(p:Predicate[A]):Predicate[A] =
    Predicate(condFn= (a:A) => {condFn.apply(a) || p.condFn.apply(a)})


  /**
   * creates a new predicate from an existing one, applying a NOT operator to the result of the source one
   * {{{
   *   val evenPred = Predicate(x:Int => x&2==0)
   *   val oddPred = evenPred.negate()
   *   val resFor8= oddPred(8)
   *   println(s"8 is odd ? = ${resFor8}") // false..
   * }}}
   * @return a new predicate built on top of the original one
   */
  def negate():Predicate[A] =
    Predicate(condFn= (a:A) => {!condFn.apply(a)})

}

object Predicates{

  /**
   * check if all predicates evaluate to true
   * {{{
   *   val pred1 = Predicate(condFn = (x: Int) => x % 2 == 0)
   *   val pred2 = Predicate(condFn = (x: Int) => x < 3)
   *   val allOfRes = allOf(List(pred1, pred2))(6)
   * }}}
   * @param preds,list of predicates to be checked
   * @param value, value to test
   * @tparam A, type of the value
   * @return true if all predicates evaluate to true for value, false otherwise
   */
  def allOf[A](preds:List[Predicate[A]])(value:A):Boolean =
    preds.map(p => p.test(value)).count(v => v) ==preds.size


  /**
   * check if all predicates evaluate to false
   * {{{
   *   val pred1 = Predicate(condFn = (x: Int) => x % 2 == 0)
   *   val pred2 = Predicate(condFn = (x: Int) => x < 3)
   *   val allOfRes = noneOf(List(pred1, pred2))(6)
   * }}}
   * @param preds,list of predicates to be checked
   * @param value, value to test
   * @tparam A, type of the value
   * @return true if all predicates evaluate to false for value, false otherwise
   */
  def  noneOf[A](preds:List[Predicate[A]])(value:A):Boolean =
    preds.map(p => p.test(value)).count(v => !v) ==preds.size


  /**
   * check if at least one predicate evaluate to true
   * {{{
   *   val pred1 = Predicate(condFn = (x: Int) => x % 2 == 0)
   *   val pred2 = Predicate(condFn = (x: Int) => x < 3)
   *   val anyOfRes = anyOf(List(pred1, pred2))(6)
   * }}}
   * @param preds,list of predicates to be checked
   * @param value, value to test
   * @tparam A, type of the value
   * @return true if one of the predicates evaluate to true, false otherwise
   */
  def anyOf[A](preds:List[Predicate[A]])(value:A):Boolean =
    preds.exists(p => p.test(value))

  def apply[A](condFn: A => Boolean):Predicate[A]={
    Predicate(condFn)
  }

  /**
   * build a predicate checking for value inside a list of expected values
   * @param values, list of possible values
   * @tparam A , type of the value to checked
   * @return true if value is present in the list, false otherwise
   */
  def isIn[A](values:List[A]):Predicate[A]  =
    Predicate(condFn = (a:A) => {
      values.exists(p = x => x==a)
    })

  def is[A](v:A):Predicate[A] =
    Predicate(condFn= (a:A) => a==v)


}

