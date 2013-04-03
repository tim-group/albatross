package com.youdevise.albatross

import org.specs2.Specification

import Intervals._
import org.specs2.matcher.Matcher

class IntervalSpec extends Specification {

  def is =
  "An interval" ^
    "cannot be created if it encloses no points" ! {
      (open(0) to open(0)) must throwAn[IllegalArgumentException]
    } ^
    "is a singleton if it encloses only one point" ! {
      (closed(0) to closed(0)) must beASingleton
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
        (closed(10) to unbounded[Int] must not(enclose(-1000)))
    } ^
    "encloses any interval whose bounds are within its own" ! {
      (open(-1) to open(11)) must encloseInterval (open(0) to open(10))
    } ^
    "does not enclose any interval either of whose bounds are outside its own" ! {
      ((open(-1) to open(5)) must not(encloseInterval(open(0) to open(10)))) and
        ((open(5) to open(11)) must not(encloseInterval(open(0) to open(10))))
    } ^ end ^
  bt ^
  "The intersection of two intervals" ^
    "Is the second interval if the first contains the second" ! {
      (open(-10) to open(10)) intersect (open(-5) to open(5)) must_== (open(-5) to open(5))
    } ^
    "Is the first interval if the second contains the first" ! {
      (open(-10) to open(10)) intersect (open(-25) to open(25)) must_== (open(-10) to open(10))
    } ^
    "Is the overlap between the two intervals if they are connected" ! {
      (open(-10) to open(10)) intersect (open(5) to open(15)) must_== (open(5) to open(10))
    } ^
    "Is a singleton interval if the two intervals abut" ! {
      (closed(-10) to closed(10)) intersect (closed(10) to closed(20)) must_== (closed(10) to closed(10))
    } ^
    "Is an open interval when one is open and the other is closed, but both have the same endpoints" ! {
      val openInterval = open(-10) to open(10)
      val closedInterval = closed(-10) to closed(10)
      openInterval intersect closedInterval must_== openInterval
      closedInterval intersect openInterval must_== openInterval
    } ^
    "Is empty if the two intervals do not touch" ! {
      ((open(0) to open(10)) intersect (open(10) to open(20))) must beEmpty
    } ^ end ^
  bt ^
  "The union of two intervals" ^
    "Is a set containing both if the intervals are not connected" ! {
      ((open(0) to open(10)) union (open(10) to open(20))) must_== Interval(open(0) to open(10), open(10) to open(20))
    } ^
    "Is a set containing a single combined interval if the intervals are connected" ! {
      ((open(0) to closed(10)) union (closed(10) to open(20))) must_== (open(0) to open(20))
    } ^ end ^
  bt ^
  "The complement of two intervals" ^
    "Is an empty set if the second encloses the first" ! {
      ((open(0) to open(10)) complement (open(-1) to open(11))) must beEmpty
    } ^
    "Is a singleton set containing the truncated first set if the second set overlaps the first" ! {
      (((open(0) to open(10)) complement (open(5) to open(15))) must_== (open(0) to closed(5))) and
        (((open(0) to open(10)) complement (open(-5) to open(5))) must_== (closed(5) to open(10)))
    } ^
    "Is a set containing a pair of separated intervals, if the first interval encloses the second" ! {
      ((open(0) to open(10)) complement (closed(3) to closed(7))) must_== Interval(open(0) to open(3), open(7) to open(10))
    } ^ end

  def beASingleton[T]: Matcher[ContinuousInterval[T]] = ((_: ContinuousInterval[T]).isASingleton, "is not a singleton")
  def enclose[T](value: T): Matcher[ContinuousInterval[T]] = ((_: ContinuousInterval[T]).encloses(value), "doesn't enclose %s".format(value))
  def encloseInterval[T](other: ContinuousInterval[T]): Matcher[ContinuousInterval[T]] = ((_: ContinuousInterval[T]).enclosesInterval(other), "doesn't enclose %s".format(other))
}
