package com.youdevise.albatross

import collection.immutable.Stream.cons

trait DiscreteDomain[T] {
  def next(value: T): T
  def previous(value: T): T
}

case class DiscreteInterval[T](intervalSet: IntervalSet[T], domain: DiscreteDomain[T]) {
  
  def toSet: Set[T] = toStream.toSet
  def toList: List[T] = toStream.toList
  def toSeq: Seq[T] = toStream.toSeq
  
  def toStream: Stream[T] =
    intervalSet.subIntervals.toStream.flatMap(toStream(_))

  private def toStream(interval: SubInterval[T]): Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.next(value)))
    stream(lowestValue(interval))
  }

  def toReverseStream: Stream[T] =
    intervalSet.subIntervals.reverse.toStream.flatMap(toReverseStream(_))

  private def toReverseStream(interval: SubInterval[T]): Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.previous(value)))
    stream(highestValue(interval))
  }

  def lowestValue(interval: SubInterval[T]): T = {
    if (interval.lower.isEmpty) throw new UnsupportedOperationException("Cannot find lowest value of unbounded interval %s".format(interval))
    val lowerBound = interval.lower.get
    if (lowerBound.isOpen) domain.next(lowerBound.endpoint) else lowerBound.endpoint
  }

  def highestValue(interval: SubInterval[T]): T = {
    if (interval.upper.isEmpty) throw new UnsupportedOperationException("Cannot find highest value of unbounded interval %s".format(interval))
    val upperBound = interval.upper.get
    if (upperBound.isOpen) domain.previous(upperBound.endpoint) else upperBound.endpoint
  }

  def lowestValue: Option[T] =
    intervalSet.subIntervals.headOption.map(lowestValue _)

  def highestValue: Option[T] =
    intervalSet.subIntervals.lastOption.map(highestValue _)
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
