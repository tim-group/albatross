package com.youdevise.albatross

import collection.immutable.Stream.cons

trait DiscreteDomain[T] {
  def next(value: T): T
  def previous(value: T): T
}

case class DiscreteInterval[T](intervalSet: IntervalSet[T], domain: DiscreteDomain[T]) {
  def toStream: Stream[T] = {
    intervalSet.nonEmptyOption match {
      case Some(nonEmpty) => nonEmpty.nonEmptyContinuousSubIntervals.toStream.flatMap(toStream(_))
      case None => Stream.empty
    }
  }

  private def toStream(interval: NonEmptyContinuousIntervalSet[T]): Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.next(value)))
    stream(lowestValue(interval))
  }

  def toReverseStream: Stream[T] = {
    intervalSet.nonEmptyOption match {
      case Some(nonEmpty) => nonEmpty.nonEmptyContinuousSubIntervals.reverse.toStream.flatMap(toReverseStream(_))
      case None => Stream.empty
    }
  }

  private def toReverseStream(interval: NonEmptyContinuousIntervalSet[T]): Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.previous(value)))
    stream(highestValue(interval))
  }

  def lowestValue(interval: NonEmptyContinuousIntervalSet[T]): T = {
    if (interval.lower.isEmpty) throw new UnsupportedOperationException("Cannot find lowest value of unbounded interval %s".format(interval))
    val lowerBound = interval.lower.get
    if (lowerBound.isOpen) domain.next(lowerBound.endpoint) else lowerBound.endpoint
  }

  def highestValue(interval: NonEmptyContinuousIntervalSet[T]): T = {
    if (interval.upper.isEmpty) throw new UnsupportedOperationException("Cannot find highest value of unbounded interval %s".format(interval))
    val upperBound = interval.upper.get
    if (upperBound.isOpen) domain.previous(upperBound.endpoint) else upperBound.endpoint
  }

  def lowestValue: T = {
    intervalSet.nonEmptyOption match {
      case Some(nonEmpty) => lowestValue(nonEmpty.nonEmptyContinuousSubIntervals.head)
      case None => throw new UnsupportedOperationException("Cannot find lowest value of empty interval")
    }
  }

  def highestValue: T = {
    intervalSet.nonEmptyOption match {
      case Some(nonEmpty) => highestValue(nonEmpty.nonEmptyContinuousSubIntervals.last)
      case None => throw new UnsupportedOperationException("Cannot find highest value of empty interval")
    }
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

  implicit def intervalSet2DiscreteInterval[T](intervalSet: IntervalSet[T])(implicit domain: DiscreteDomain[T]) =
    DiscreteInterval(intervalSet, domain)
}
