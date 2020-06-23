package lectures.part3fp

import scala.util.Random

object Sequences extends App {
  // Seq
  val aSequence = Seq(1,2,3,4)
  println(aSequence) //=> List(1,2,3,4) // companion object is List, but type is Seq[Int]
  println(aSequence.reverse) //=> List(4,3,2,1)
  println(aSequence(2)) // => 3
  println(aSequence ++ Seq(7,5,6)) // => List(1,2,3,4,7,5,6)
  println(aSequence ++ Seq(7,5,6).sorted) // => List(1,2,3,4,5,6,7)

  // Ranges
  val aRange: Seq[Int] = 1 until 10 // from 1 to 9
  aRange.foreach(println)

  (1 to 10).foreach(x => println(x.toString + ".Hello")) //from 1 to 10
  //  (1 to 10) foreach println

  // lists
  val aList = List(1,2,3)
  val prepended = 42 +: aList
  val appended = aList :+ 89
  println(prepended) // List(42, 1, 2, 3)
  println(appended) // List(42, 1, 2, 3, 89)

  val apples5 = List.fill(5)("apple")
  println(apples5)
  println(aList.mkString("-|-"))

  // arrays
  val numbers = Array(1,2,3,4)
  val threeElements = Array.ofDim[String](3)
  println(threeElements) // =>
  threeElements.foreach(println) // => '0' for Double, Float, Int, 'false' for Boolean, 'null' for String (and other reference types)

  // mutation
  numbers(2) = 0  // syntax sugar for numbers.update(2, 0)
  println(numbers.mkString(" "))

  // arrays and seq
  val numbersSeq: Seq[Int] = numbers  // implicit conversion
  println(numbersSeq) // => WrappedArray(1,2,0,4) // special type WrappedArray

  // vectors
  val vector: Vector[Int] = Vector(1,2,3)
  println(vector)

  // vectors vs lists

  val maxRuns = 1000
  val maxCapacity = 1000000

  def getWriteTime(collection: Seq[Int]): Double = {
    val r = new Random
    val times = for {
      it <- 1 to maxRuns
    } yield {
      val currentTime = System.nanoTime()
      collection.updated(r.nextInt(maxCapacity), r.nextInt())
      System.nanoTime() - currentTime
    }

    times.sum * 1.0 / maxRuns
  }

  val numbersRange = 1 to maxCapacity
  val numbersList = numbersRange.toList
  val numbersVector = numbersRange.toVector

  // keeps reference to tail
  // updating an element in the middle takes long
  println(getWriteTime(numbersList))
  // depth of the tree is small
  // needs to replace an entire 32-element chunk
  println(getWriteTime(numbersVector))
}
