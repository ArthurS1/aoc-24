package exercise

object Main {

  def main(arg: Array[String]) = {
    println("hello AOC 24 !")
  }

}

object Day1 {

  /* Exercise 1 */

  def exercise1(l1: List[Int], l2: List[Int]): Int =
    distance(distanceList(sortPair(l1, l2)))

  def sortPair(l1: List[Int], l2: List[Int]): List[(Int, Int)] =
    l1.sorted zip l2.sorted

  def distanceList(lists: List[(Int, Int)]): List[Int] =
    lists map { case (a, b) => (a - b).abs }

  def distance(list: List[Int]): Int =
    list.reduce((n, acc) => acc + n)

  /* Exercise 2 */

  def exercise2(l1: List[Int], l2: List[Int]): Int =
    total(scores(l1, l2))

  def appearsNTimes(n: Int, l: List[Int]): Int =
    l.count((e) => e == n)

  def scores(l1: List[Int], l2: List[Int]) : List[Int] =
    l1.map(a => a * appearsNTimes(a, l2))

  def total(list: List[Int]): Int =
    list.reduce((n, acc) => acc + n)
}
