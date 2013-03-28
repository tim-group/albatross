package com.youdevise.albatross

import org.specs2.Specification

import Intervals._
import Discrete._

class DiscreteSpec extends Specification {

  def is =
  "A range in a discrete domain" ^
    "has a lowest value" ! {
      (open('a') to open('e')).lowestValue must_== 'b'
    } ^
    "has a highest value" ! {
      (open('a') to open('e')).highestValue must_== 'd'
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
