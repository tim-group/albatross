package com.youdevise.albatross

import org.specs2.Specification

import Bounds._
import Discrete._

class DiscreteSpec extends Specification {

  def is =
  "A range in a discrete domain" ^
    "has a lowest value if lower-bounded" ! {
      (open('a') to open('e')).lowestValue must beSome('b')
    } ^
    "has a highest value if upper-bounded" ! {
      (open('a') to open('e')).highestValue must beSome('d')
    } ^
    "has no lowest value if lower-unbounded" ! {
      (unbounded[Char] to open('a')).lowestValue must beNone
    } ^
    "has no highest value if upper-unbounded" ! {
      (open('a') to unbounded[Char]).highestValue must beNone
    } ^
    "can be streamed over" ! {
      (closed('a') to closed('e')).toStream.toList must_== List('a', 'b', 'c', 'd', 'e')
    } ^
    "can be streamed over in reverse order" ! {
      (closed('a') to closed('e')).toReverseStream.toList must_== List('e', 'd', 'c', 'b', 'a')
    } ^
    "can be streamed over indefinitely if upper-unbounded" ! {
      (closed(0) to unbounded[Int]).toStream.apply(10000) must_== 10000
    } ^
    "can be reverse streamed over indefinitely if lower-unbounded" ! {
      (unbounded[Int] to closed(0)).toReverseStream.apply(10000) must_== -10000
    } ^ end

}
