package lectures.part3fp

import scala.annotation.tailrec

object TuplesAndMaps extends App {
  // tuples = finite ordered "lists"
  val aTuple = (2, "hello, Scala")  // (new) Tuple2[Int, String] = (Int, String)
  println(2 -> "Hello, Scala") // eq to (2, "Hello, Scala") [sugar for tuple]

  println(aTuple._1)  // 2
  println(aTuple.copy(_2 = "goodbye Java")) // (2, "goodbye Java")
  println(aTuple.swap)  // ("hello, Scala", 2)

  // Maps - keys -> values
  val aMap: Map[String, Int] = Map()

  val phonebook = Map(("Jim", 555), "Daniel" -> 789, ("JIM", 9000)).withDefaultValue(-1)
  // a -> b is sugar for (a, b)
  println(phonebook) // Map(Jim -> 555, Daniel -> 789, JIM -> 9000)

  // map ops
  println(phonebook.contains("Jim")) // true
  println(phonebook("Mary")) // -1

  // add a pairing
  val newPairing = "Mary" -> 678
  val newPhonebook = phonebook + newPairing
  println(newPhonebook) // Map(Jim -> 555, Daniel -> 789, JIM -> 9000, Mary -> 678)

  // maps functions like map, flatMap, filter
  println(phonebook.map(pair => pair._1.toLowerCase -> pair._2)) // Map(jim -> 9000, daniel -> 789)

  // filterKeys !! Map().filterKeys is deprecated
  // because Map().view.filterKeys() => MapView
  println(phonebook.view.filterKeys(x => x.startsWith("J")).toMap) //  Map(Jim -> 555, JIM -> 9000)

  // mapValues !! Map().mapValues is deprecated
  // because Map().view.filterKeys() => MapView
  println(phonebook.view.mapValues(number => "0245-" + number).toMap) // Map(Jim -> 0245-555, Daniel -> 0245-789, JIM -> 0245-9000)

  // conversions to other collections
  println(phonebook.toList) // List((Jim,555), (Daniel,789), (JIM,9000))
  println(List(("Daniel", 555)).toMap) // Map(Daniel -> 555)
  val names = List("Bob", "James", "Angela", "Mary", "Daniel", "Jim")
  println(names.groupBy(name => name.charAt(0))) // HashMap(J -> List(James, Jim), A -> List(Angela), M -> List(Mary), B -> List(Bob), D -> List(Daniel))

  /*
    1.  What would happen if I had two original entries "Jim" -> 555 and "JIM" -> 900
        !!! careful with mapping keys. "Jim".toLowerCase() -> 555 overlapped by "JIM".toLowerCase() -> 900
   */
}
