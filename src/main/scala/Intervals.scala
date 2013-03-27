package com.youdevise.albatross

object Intervals {
  def open[T](endpoint: T)(implicit ordering: Ordering[T]) = OpenBoundBuilder[T](endpoint)
  def closed[T](endpoint: T)(implicit ordering: Ordering[T]) = ClosedBoundBuilder[T](endpoint)
  def unbounded[T]()(implicit ordering: Ordering[T]) = UnboundedBuilder[T]
}

trait Interval[T] {
  val isEmpty: Boolean
  def encloses(value: T): Boolean
  def enclosesBound(bound: Bound[T]): Boolean
  def enclosesInterval(other: Interval[T]): Boolean
  def connectedTo(other: Interval[T]): Boolean
  def intersect(other: Interval[T]): Interval[T]
  def union(other: Interval[T]): Set[NonEmptyInterval[T]]
  def complement(other: Interval[T]): Set[NonEmptyInterval[T]]
}

object Interval {
  def empty[T] = new EmptyInterval[T]
  def apply[T](lower: Bound[T] with LowerBound[T], upper: Bound[T] with UpperBound[T]): Interval[T] =
    (lower, upper) match {
      case (boundedLower: Bounded[T], boundedUpper: Bounded[T])
        if !(boundedLower encloses boundedUpper.endpoint) ||
          !(boundedUpper encloses boundedLower.endpoint) => empty[T]
      case _                                             => new NonEmptyInterval(lower, upper)
    }
}

sealed case class EmptyInterval[T]() extends Interval[T] {
  override val isEmpty = true
  override def encloses(value: T) = false
  override def enclosesBound(bound: Bound[T]) = false
  override def enclosesInterval(other: Interval[T]) = false
  override def connectedTo(other: Interval[T]) = false
  override def intersect(other: Interval[T]) = this
  override def union(other: Interval[T]): Set[NonEmptyInterval[T]] = other match {
    case nonEmpty @ NonEmptyInterval(_, _) => Set(nonEmpty)
    case _ => Set.empty[NonEmptyInterval[T]]
  }
  override def complement(other: Interval[T]): Set[NonEmptyInterval[T]] = Set.empty[NonEmptyInterval[T]]
}

sealed case class NonEmptyInterval[T](lower: Bound[T] with LowerBound[T], upper: Bound[T] with UpperBound[T]) extends Interval[T] {
  override val isEmpty = false

  override def encloses(value: T) = lower.encloses(value) && upper.encloses(value)

  override def enclosesBound(bound: Bound[T]) = bound match {
    case Bounded(endpoint: T) => encloses(endpoint)
    case _ => false
  }

  override def enclosesInterval(other: Interval[T]) = (this intersect other) == other

  override def connectedTo(other: Interval[T]) = other match {
    case EmptyInterval() => false
    case NonEmptyInterval(otherLower, otherUpper) =>
      (this enclosesBound otherLower) || (this enclosesBound otherUpper) ||
        (other enclosesBound lower) || (other enclosesBound upper)
  }

  override def intersect(other: Interval[T]): Interval[T] = other match {
    case NonEmptyInterval(otherLower, otherUpper) if connectedTo(other) => 
      Interval(lower.innerMost(otherLower), upper.innerMost(otherUpper))
    case _ => Interval.empty[T]
  }

  override def union(other: Interval[T]): Set[NonEmptyInterval[T]] = other match {
    case nonEmpty @ NonEmptyInterval(otherLower, otherUpper) =>
      if (connectedTo(nonEmpty)) Set(NonEmptyInterval(lower.outerMost(otherLower), upper.outerMost(otherUpper)))
      else Set(this, nonEmpty)
    case _ => Set(this)
  }

  override def complement(other: Interval[T]): Set[NonEmptyInterval[T]] = other match {
    case NonEmptyInterval(otherLower, otherUpper) =>
      if (this == other) Set.empty[NonEmptyInterval[T]]
      else if (!connectedTo(other)) Set(this)
      else if (enclosesInterval(other)) Set(NonEmptyInterval(lower, otherLower.inverse), NonEmptyInterval(otherUpper.inverse, upper))
      else if (other.enclosesInterval(this)) Set.empty[NonEmptyInterval[T]]
      else if (enclosesBound(otherLower)) Set(NonEmptyInterval(lower, otherLower.inverse))
      else Set(NonEmptyInterval(otherUpper.inverse, upper))
    case EmptyInterval() => Set(this)
  }
}

sealed case class IntervalSet[T](intervals: Set[Interval[T]]) {
  implicit val intervalOrder = (x: NonEmptyInterval[T], y: NonEmptyInterval[T]) =>
      if (x.lower == y.lower) (x.upper != y.upper && x.upper.innerMost(y.upper) == x.upper)
      else (x.lower.outerMost(y.lower) == x.lower)

  lazy val coalesced: List[Interval[T]] = {
    val nonEmpty = intervals.filter(_.isEmpty == false).map(_.asInstanceOf[NonEmptyInterval[T]])
    if (nonEmpty.size < 2) nonEmpty.toList
    else {
      val ordered = nonEmpty.toList.sortWith(intervalOrder)
      var left = ordered.head
      val iter = ordered.iterator.drop(1)
      val result = collection.mutable.LinkedHashSet[Interval[T]]()
      while (iter.hasNext) {
        val right = iter.next()
        if (left connectedTo right)
          left = NonEmptyInterval(left.lower.outerMost(right.lower), left.upper.outerMost(right.upper))
          else {
            result add left
            left = right
          }
      }
      result add left
      List(result.toSeq:_*)
    }
  }
}
