package com.youdevise.albatross

import collection.immutable.Stream.cons

trait DiscreteDomain[T] {
  def next(value: T): T
  def previous(value: T): T
}

case class DiscreteInterval[T](intervalSet: IntervalSet[T], domain: DiscreteDomain[T]) {
  
  def toSet: Set[T] =
    if (isBounded) toStream.toSet else throw new UnsupportedOperationException("Cannot convert unbounded interval to set")
  def toList: List[T] =
    if (isBounded) toStream.toList else throw new UnsupportedOperationException("Cannot convert unbounded interval to list")
  def toSeq: Seq[T] =
    if (isBounded) toStream.toSeq else throw new UnsupportedOperationException("Cannot convert unbounded interval to seq")

  def isBounded = lowestValue.isDefined && highestValue.isDefined
  
  def toStream: Stream[T] =
    intervalSet.subIntervals.toStream.flatMap(toStream(_))

  private def toStream(interval: SubInterval[T]): Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.next(value)))
    lowestValue(interval) match {
      case Some(value) => stream(value)
      case None        => throw new UnsupportedOperationException("Cannot stream over interval with no lower bound")
    }
  }

  def toReverseStream: Stream[T] =
    intervalSet.subIntervals.reverse.toStream.flatMap(toReverseStream(_))

  private def toReverseStream(interval: SubInterval[T]): Stream[T] = {
    def stream(value: T): Stream[T] = if (!interval.encloses(value)) Stream.empty else cons(value, stream(domain.previous(value)))
    highestValue(interval) match {
      case Some(value) => stream(value)
      case None        => throw new UnsupportedOperationException("Cannot reverse stream over interval with no upper bound")
    }
  }

  def lowestValue(interval: SubInterval[T]): Option[T] =
    interval.lower.map(lowerBound =>
      if (lowerBound.isOpen) domain.next(lowerBound.endpoint) else lowerBound.endpoint
    )

  def highestValue(interval: SubInterval[T]): Option[T] =
    interval.upper.map(upperBound =>
      if (upperBound.isOpen) domain.previous(upperBound.endpoint) else upperBound.endpoint
    )

  def lowestValue: Option[T] =
    intervalSet.subIntervals.headOption.flatMap(lowestValue _)

  def highestValue: Option[T] =
    intervalSet.subIntervals.lastOption.flatMap(highestValue _)
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
