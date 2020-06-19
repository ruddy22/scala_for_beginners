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
      .map(new Function1[Int, Int] {
        override def apply(el: Int): Int = el * 2
      }) // MyList [2, 4]
      .filter(new Function1[Int, Boolean] {
        override def apply(el: Int): Boolean = el == 2
      }) // MyList [2]
      .flatMap(new Function1[Int, MyList[Int]] {
        override def apply(el: Int): MyList[Int] = Cons(el, Cons(el + 1, EmptyList))
      }) // MyList [2, 3]
      .toString
  )
  println(
    myListOfInt
      .map(new Function1[Int, Int] {
        override def apply(el: Int): Int = el * 2
      }) // MyList [2, 4]
      .filter(new Function1[Int, Boolean] {
        override def apply(el: Int): Boolean = el == 2
      }) // MyList [2]
      .map(new Function1[Int, MyList[Int]] {
        override def apply(el: Int): MyList[Int] = Cons(el, Cons(el + 1, EmptyList))
      }) // MyList [MyList [2, 3]] <- нет разворачивания внутренних списков
      .toString
  )
  println(myListOfStr.toString)

  println(cloneOfMyListOfInt == myListOfInt) // true, because case class
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
