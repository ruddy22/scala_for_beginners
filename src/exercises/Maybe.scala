package exercises

import javax.xml.crypto.dsig.Transform

sealed trait Maybe[+T] {
  def map[B](transform: T => B): Maybe[B]
  def flatMap[B](transform: T => Maybe[B]): Maybe[B]
  def filter(predicate: T => Boolean): Maybe[T]
}

// in Daniels course that object has other name (MaybeNot)
case object Nil extends Maybe[Nothing] {
  def map[B](transform: Nothing => B): Maybe[B] = Nil
  def filter(transform: Nothing => Boolean): Maybe[Nothing] = Nil
  def flatMap[B](predicate: Nothing => Maybe[B]): Maybe[B] = Nil
}

case class Just[+T](value: T) extends Maybe[T] {
  def map[B](transform: T => B): Maybe[B] = Just(transform(value))
  def flatMap[B](transform: T => Maybe[B]): Maybe[B] = transform(value)
  def filter(predicate: T => Boolean): Maybe[T] = if (predicate(value)) Just(value) else Nil
  // Daniels implementation of filter
  // def filter(p: T => Boolean): Maybe[T] = if (p(value)) this else Nil
}

object TestMaybe extends App {
  val one = Just(1)
  val two = Just(2)
  println(one.filter(_ % 2 == 0).map(_ * 2)) // => Nil
  println(two.filter(_ % 2 == 0).map(_ * 2)) // Just(4)

  val just3 = Just(3)
  println(just3) // Just(3)
  println(just3.map(_ * 2)) // Just(6)
  println(just3.flatMap(x => Just(x * 3))) // Just(9)
  println(just3.flatMap(x => Just(x % 2 == 0))) // Just(false)
  println(just3.filter(_ % 2 == 0)) // Nil
}
