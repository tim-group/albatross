package com.youdevise.albatross

import org.specs2.Specification

import Intervals._
import org.specs2.matcher.Matcher

class IntervalSpec extends Specification {

  def is =
  "An interval" ^
    "is empty if it encloses no points" ! {
      open(0) to closed(0) must beEmpty
    } ^
    "encloses any point within its bounds" ! {
      (open(10) to closed(20) must enclose(15)) and
        (unbounded[Int] to closed(20) must enclose(-1000)) and
        (closed(10) to unbounded[Int] must enclose(1000)) and
        (closed(10) to closed(10) must enclose(10))
    } ^
    "does not enclose any point outside its bounds" ! {
      (open(10) to closed(20) must not(enclose(10))) and
        (unbounded[Int] to closed(20) must not(enclose(1000))) and
        (closed(10) to unbounded[Int] must not(enclose(-1000))) and
        (open(10) to open(10) must not(enclose(10)))
    } ^ end ^
  bt ^
  "The intersection of two intervals" ^
    "Is the second interval if the first contains the second" ! {
      (open(-10) to open(10)) intersect (open(-5) to open(5)) must_== Some(open(-5) to open(5))
    } ^
    "Is the first interval if the second contains the first" ! {
      (open(-10) to open(10)) intersect (open(-25) to open(25)) must_== Some(open(-10) to open(10))
    } ^
    "Is the overlap between the two intervals if they are connected" ! {
      (open(-10) to open(10)) intersect (open(5) to open(15)) must_== Some(open(5) to open(10))
    } ^
    "Is a singleton interval if the two intervals abut" ! {
      (closed(-10) to closed(10)) intersect (closed(10) to closed(20)) must_== Some(closed(10) to closed(10))
    } ^
    "Is an open interval when one is open and the other is closed, but both have the same endpoints" ! {
      val openInterval = open(-10) to open(10)
      val closedInterval = closed(-10) to closed(10)
      openInterval intersect closedInterval must_== Some(openInterval)
      closedInterval intersect openInterval must_== Some(openInterval)
    } ^
    "Is empty if the two intervals do not touch" ! {
      ((open(0) to open(10)) intersect (open(10) to open(20))) must beNone
    } ^
    "The union of two intervals" ^
      "Is a set containing both if the intervals are not connected" ! {
        ((open(0) to open(10)) union (open(10) to open(20))) must_== Set(open(0) to open(10), open(10) to open(20))
      } ^
      "Is a set containing a single combined interval if the intervals are connected" ! {
        ((open(0) to closed(10)) union (closed(10) to open(20))) must_== Set(open(0) to open(20))
      } ^ end

  def enclose[T](value: T): Matcher[Interval[T]] = ((_: Interval[T]).encloses(value), "doesn't enclose %s".format(value))
}
