package com.youdevise.albatross

import org.specs2.Specification
import Intervals._
import Discrete._

class DiscreteSpec extends Specification {
  def is =
  "An interval defined on a discrete type" ^
    "Can be iterated over" ! {
      (open(0) to closed(10)).toList must_== List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    } ^
    "Can be reverse iterated over" ! {
      (unbounded[Int] to open(10)).backwards.take(5).toList must_== List(9, 8, 7, 6, 5)
    } ^
    "Can be stepped over" ! {
      (open(0) to closed(10) by 2).toList must_== List(2, 4, 6, 8, 10)
    } ^
    "Can be stepped over in reverse" ! {
      ((open(0) to closed(10)).backwards by 2).toList must_== List(10, 8, 6, 4, 2)
    } ^ end
}
