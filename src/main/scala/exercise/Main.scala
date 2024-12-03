package exercise

import scala.annotation.tailrec
import scala.collection.immutable

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

  def scores(l1: List[Int], l2: List[Int]): List[Int] =
    l1.map(a => a * appearsNTimes(a, l2))

  def total(list: List[Int]): Int =
    list.reduce((n, acc) => acc + n)
}

object Day2 {

  /* Exercise 1 */

  type Level = Int
  type Report = List[Level]

  def exercise1(reports: List[Report]): Int =
    reports
      .map(safe(_))
      .count(identity(_))

  def safe(report: Report): Boolean =
    (decreasingOrIncreasing(report), adjacency(report)) match {
      case (Increasing(), true) => true
      case (Decreasing(), true) => true
      case _                    => false
    }

  @tailrec
  def adjacency(report: Report, acc: Boolean = true): Boolean =
    report match {
      case a :: b :: Nil if acc  => adjacent(a, b)
      case a :: b :: next if acc => adjacency(b :: next, adjacent(a, b))
      case _                     => false
    }

  def adjacent(a: Level, b: Level): Boolean = {
    val diff = (a - b).abs

    diff >= 1 && diff <= 3
  }

  sealed trait ReportTendency

  case class Undefined() extends ReportTendency
  case class Increasing() extends ReportTendency
  case class Decreasing() extends ReportTendency

  def decreasingOrIncreasing(report: Report): ReportTendency =
    if (report.sorted == report) Increasing()
    else if (report.sorted == report.reverse) Decreasing()
    else Undefined()

  /* Exercise 2 */

  def exercise2(reports: List[Report]): Int =
    reports
      .map(report => {
        if (safe(report))
          true
        else
          problemDampener(report)
      })
      .count(identity(_))

  def problemDampener(report: Report): Boolean =
    dampening(report).find(safe(_)).isDefined

  def dampening(report: Report): List[Report] =
    Range(0, report.size)
      .map(report.splitAt(_) match {
        case (l1, elem :: l2) => l1 ::: l2
      })
      .toList

}
