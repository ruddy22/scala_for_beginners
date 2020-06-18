package lectures.part1basics

import scala.io.StdIn

object ValuesVariablesTypes extends App {
  val x = 42
  println(x)

  // vals are immutable
  // x = 24 /// => error
  // x += 1 /// => error

  // compiler can infer the types

  val aString: String = "String" ; val anotherString = "anotherString"
  val bool: Boolean = true
  val aChar: Char = 'x'
  val aInt: Int = x
  val aShort: Short = 9999
  val aLong: Long = 9L
  val aFloat: Float = 2.0f
  val aDouble: Double = 3.14

  // variables
  var aVariable: Int = 4
  println(aVariable)
  aVariable = 5
  println(aVariable)
  aVariable +=  1
  println(aVariable)

  // variables are mutable
}
