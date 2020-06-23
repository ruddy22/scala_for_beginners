package exercises

trait MyList[+A] {
  def head: A
  def isEmpty: Boolean
  def tail: MyList[A]
  def add[B >: A](el: B): MyList[B]
  def printElements: String
  override def toString: String = "MyList [" + printElements + "]"
  def map[B](transformer: A => B): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  // lists concatination
  def ++[B >: A](lst: MyList[B]): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]

  // hofs
  def foreach(fn: A => Unit): Unit
  def sort(fn: (A,  A) => Int): MyList[A]
  def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C]
  def fold[B](start: B)(fn: (B, A) => B): B
}

case object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def add[B >: Nothing](el: B): MyList[B] = Cons(el, EmptyList)
  def printElements: String = ""
  def map[B](transformer: Nothing => B): MyList[B] = EmptyList
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = EmptyList
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = EmptyList
  def ++[B >: Nothing](lst: MyList[B]): MyList[B] = lst

  def foreach(fn: Nothing => Unit): Unit = ()
  def sort(fn: (Nothing, Nothing) => Int): MyList[Nothing] = EmptyList
  def zipWith[B, C](list: MyList[B], fn: (Nothing, B) => C): MyList[C] =
    if (!list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else EmptyList
  def fold[B](start: B)(fn: (B, Nothing) => B): B = start
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def isEmpty: Boolean = false
  def tail: MyList[A] = t
  def add[B >: A](el: B): MyList[B] = Cons(el, this)
  def printElements: String = if (t.isEmpty) h.toString else h.toString + ", " + t.printElements
  def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h)) Cons(h, t.filter(predicate))
    else t.filter(predicate)
  def map[B](transformer: A => B): MyList[B] =
    Cons(transformer(h), t.map(transformer))

  /**
   * [1, 2] ++ [3, 4]
   * => new Cons(1, [2] ++ [3, 4])
   * => new Cons(1, new Cons(2, EmptyList ++ [3, 4]))
   * => new Cons(1, new Cons(2, [3, 4]))
   * => new Cons(1, new Cons(2, new Cons(3, new Cons(4, EmptyList))))
   */
  def ++[B >: A](lst: MyList[B]): MyList[B] = Cons(h, t ++ lst)

  /**
   * [1, 2] flatMap ([n, n+1])
   * => [1, 2] ++ [2] flatMap [n, n+1]
   * => [1, 2] ++ [2, 3]
   * => [1, 2, 2, 3]
   * => new Cons(1, new Cons(2, new Cons(2, new Cons(3, EmptyList))))
   */
  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  def foreach(fn: A => Unit): Unit = {
    fn(h)
    t.foreach(fn)
  }
  def sort(fn: (A,  A) => Int): MyList[A] = {
    // TODO: make the tail recursion version
    def insert(x: A, sortedList: MyList[A]): MyList[A] =
      if (sortedList.isEmpty) Cons(x, EmptyList)
      else if (fn(x, sortedList.head) < 0) Cons(x, sortedList)
      else Cons(sortedList.head, insert(x, sortedList.tail))

    val sortedTail = t.sort(fn)
    insert(h, sortedTail)
  }
  def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] =
    if (list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else Cons(zip(h, list.head), t.zipWith(list.tail, zip))

  /**
   * [1,2,3].fold(0)(+) =>
   * [2,3].fold(1)(+) =>
   * [3].fold(3)(+) =>
   * [].fold(6)(+) =>
   * 6
   */
  def fold[B](start: B)(fn: (B, A) => B): B =
    t.fold(fn(start, h))(fn)
}

object TestMyList extends App {
//  val list = new Cons(1, new Cons(2, new Cons(3, EmptyList)))
//
//  println(list.isEmpty)
//  println(list.head)
//  println(list.tail.head)
//
//  println(list.add(5).head)
//  println(list.toString)
//
//  val emptyList = EmptyList
//  println(emptyList.isEmpty)
//  println(emptyList.toString)
//  println(emptyList.add(1).head)

  val myListOfInt: MyList[Int] = Cons(1, Cons(2, EmptyList))
  val cloneOfMyListOfInt: MyList[Int] = Cons(1, Cons(2, EmptyList))
  val myListOfStr: MyList[String] = Cons("Hello", Cons("Scala", EmptyList))

  println(myListOfInt.toString)
  println(
    myListOfInt
      .map(_ * 2) // MyList [2, 4]
      .filter(_ == 2) // MyList [2]
      .flatMap(el => Cons(el, Cons(el + 1, EmptyList))) // MyList [2, 3]
      .toString
  )
  println(
    myListOfInt
      .map(_ * 2) // MyList [2, 4]
      .filter(_ == 2) // MyList [2]
      .map(el => Cons(el, Cons(el + 1, EmptyList))) // MyList [MyList [2, 3]] <- нет разворачивания внутренних списков
      .toString
  )
  println(myListOfStr.toString)

  println(cloneOfMyListOfInt == myListOfInt) // true, because case class
  myListOfInt.foreach(println)
  println(myListOfInt.sort((x, y) => y - x))
  println(myListOfStr.zipWith[Int, String](myListOfInt, _ + "+" + _)) //=> MyList [Hello+1, Scala+2]
  println(myListOfInt.fold(0)(_ + _)) // => 3
  println(myListOfStr.fold("")(_ ++ _)) // => HelloScala

  // MyList is compatible with for-comprehensions
  // because it have map, filter, flatMap
  for {
    n <- myListOfInt
    str <- myListOfStr
  } println(n.toString + '-' + str)
  // print on each line
  // 1-Hello
  // 1-Scala
  // 2-Hello
  // 2-Scala

  val combinations = for {
    n <- myListOfInt
    str <- myListOfStr
  } yield n.toString + '-' + str
  println(combinations)
  // print on one line
  // MyList [1-Hello, 1-Hello, 2-Hello, 2-Scala]
  // omg, this is MyList instance (so, flatMap in action)
}


//class MyList(args: Array[Int] = Array()) extends MyList {
//  def isEmpty: Boolean = args.length == 0
//  def head: Int = if (this.isEmpty) throw new NoSuchElementException else args(0)
//  def tail: MyList = if (this.isEmpty) throw new NoSuchElementException else new MyList(args.slice(1, args.length + 1))
//  def add(int: Int): MyList = new MyList((args :+ int).toArray)
//  override def toString: String = s"MyList(${for (element <- args) yield element.toString})"
//}

//object MyList {
//  private def apply(args: Array[Int]): MyList = {
//    new MyList(args)
//  }
//}
