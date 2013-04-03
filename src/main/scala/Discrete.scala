package com.youdevise.albatross

import collection.immutable.Stream.cons

trait DiscreteDomain[T] {
  def next(value: T): T
  def previous(value: T): T
}

case class DiscreteInterval[T](interval: ContinuousInterval[T], domain: DiscreteDomain[T]) {
  def toStream: Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.next(value)))
    stream(lowestValue)
  }

  def toReverseStream: Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.previous(value)))
    stream(highestValue)
  }

  def lowestValue: T = {
    if (interval.lower.isEmpty) throw new UnsupportedOperationException("Cannot find lowest value of unbounded interval %s".format(interval))
    val lowerBound = interval.lower.get
    if (lowerBound.isOpen) domain.next(lowerBound.endpoint) else lowerBound.endpoint
  }

  def highestValue: T = {
    if (interval.upper.isEmpty) throw new UnsupportedOperationException("Cannot find highest value of unbounded interval %s".format(interval))
    val upperBound = interval.upper.get
    if (upperBound.isOpen) domain.previous(upperBound.endpoint) else upperBound.endpoint
  }
}

object Discrete {

  implicit object IntDomain extends DiscreteDomain[Int] {
    def next(value: Int) = value + 1
    def previous(value: Int) = value - 1
  }

  implicit object CharDomain extends DiscreteDomain[Char] {
    override def next(value: Char) = (value.toInt + 1).toChar
    override def previous(value: Char) = (value.toInt - 1).toChar
  }

  implicit def interval2DiscreteInterval[T](interval: ContinuousInterval[T])(implicit domain: DiscreteDomain[T]) = DiscreteInterval(interval, domain)
}
