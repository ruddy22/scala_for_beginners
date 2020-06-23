package exercises

import scala.annotation.tailrec

object SocialNetworkExample extends App {

  /*
  2.  Overly simplified social network based on maps
      Person = String
    - add a person to the network
    - remove
    - friend (mutual)
    - unfriend
    - number of friends of a person
    - person with most friends
    - how many people have NO friends
    - if there is a social connection between two people (direct or not)
  */
  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def friend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a) // getting from map value by key (Bob's friends), for example : "Bob" -> Set()
    val friendsB = network(b)

    network + (a -> (friendsA + b)) + (b -> (friendsB + a))
  }

  def unfriend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)

    network + (a -> (friendsA - b)) + (b -> (friendsB - a))
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] =
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))

    val unfriended = removeAux(network(person), network) // remove persons friendships from network
    unfriended - person // remove person from network
  }

  val empty: Map[String, Set[String]] = Map()

  // Bob, Mary example
  val network = add(add(empty, "Bob"), "Mary")

  println(network) // Map(Bob -> Set(), Mary -> Set())
  println(friend(network, "Bob", "Mary")) // Map(Bob -> Set(Mary), Mary -> Set(Bob))
  println(unfriend(friend(network, "Bob", "Mary"), "Bob", "Mary")) // Map(Bob -> Set(), Mary -> Set())
  println(remove(friend(network, "Bob", "Mary"), "Bob")) //  Map(Mary -> Set())

  // Jim,Bob,Mary example
  val people = add(add(add(empty, "Bob"), "Mary"), "Jim")
  val jimBob = friend(people, "Bob", "Jim")
  val testNet = friend(jimBob, "Bob", "Mary")

  println(testNet) // Map(Bob -> Set(Jim, Mary), Mary -> Set(Bob), Jim -> Set(Bob))

  def nFriends(network: Map[String, Set[String]], person: String): Int =
    if (!network.contains(person)) 0
    else network(person).size

  println(nFriends(testNet, "Bob")) // 2

  //  def mostFriends(network: Map[String, Set[String]]): String =
  //    network.maxBy(pair => pair._2.size)._1
  def mostFriends(network: Map[String, Set[String]]): String = {
    val personWithMaxFriends = network.maxBy( pair => pair._2.size) // compare size/length (friends) for each key (person)
    personWithMaxFriends._1 // return a name of person with max friends count
  }

  println(mostFriends(testNet)) // Bob

  def nPeopleWithNoFriends(network: Map[String, Set[String]]): Int =
    network.count(_._2.isEmpty) // eq to network.view.filterKeys(k => network(k).size == 0).size

  println(nPeopleWithNoFriends(testNet)) // 0
  println(nPeopleWithNoFriends(add(testNet, "Natan"))) // 1, because "Natan" doesn't have friends

  def socialConnection(network: Map[String, Set[String]], sourcePerson: String, targetPerson: String): Boolean = {
    @tailrec
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if (discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if (person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople.tail ++ network(person))
      }
    }

    bfs(targetPerson, Set(), network(sourcePerson) + sourcePerson)
  }

  println(socialConnection(testNet, "Mary", "Jim")) // true
  println(socialConnection(network, "Mary", "Bob")) // false
}
