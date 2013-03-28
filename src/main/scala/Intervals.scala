package com.youdevise.albatross

import Bounds._

object Intervals {
  def open[T](endpoint: T)(implicit ordering: Ordering[T]) = OpenBoundBuilder[T](endpoint)
  def closed[T](endpoint: T)(implicit ordering: Ordering[T]) = ClosedBoundBuilder[T](endpoint)
  def unbounded[T]()(implicit ordering: Ordering[T]) = UnboundedBuilder[T]
}

sealed case class Interval[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]) {
  val isEmpty = (for {
    boundedLower <- lower
    boundedUpper <- upper
  } yield !(boundedLower.encloses(boundedUpper.endpoint) && boundedUpper.encloses(boundedLower.endpoint))).getOrElse(false)

  def encloses(value: T): Boolean = lower.map(_.encloses(value)).getOrElse(true) && upper.map(_.encloses(value)).getOrElse(true)

  def enclosesBound(bound: Option[Bound[T]]): Boolean = bound.map(b => encloses(b.endpoint)).getOrElse(false)

  def enclosesInterval(other: Interval[T]): Boolean = (this intersect other) == other

  def connectedTo(other: Interval[T]): Boolean =
    if (other.isEmpty) false
    else (this enclosesBound other.lower) || (this enclosesBound other.upper) ||
         (other enclosesBound lower) || (other enclosesBound upper)

  def intersect(other: Interval[T]): Option[Interval[T]] =
    if (!connectedTo(other)) None
    else Some(Interval(greatestLower(lower, other.lower), leastUpper(upper, other.upper)))

  def union(other: Interval[T]): Set[Interval[T]] =
    if (other.isEmpty) Set(this)
    else if (connectedTo(other)) Set(Interval(leastLower(lower, other.lower), greatestUpper(upper, other.upper)))
    else Set(this, other)


  def complement(other: Interval[T]): Set[Interval[T]] =
    if (other.isEmpty) Set(this)
    else if (this == other) Set.empty[Interval[T]]
    else if (!connectedTo(other)) Set(this)
    else if (other enclosesInterval this) Set.empty[Interval[T]]
    else if (this enclosesInterval other) Set(
      Interval(lower, other.lower.map(_.inverse)),
      Interval(other.upper.map(_.inverse), upper))
    else if (this enclosesBound other.lower) Set(Interval(lower, other.lower.map(_.inverse)))
    else Set(Interval(other.upper.map(_.inverse), upper))

}

sealed case class IntervalSet[T](intervals: Set[Interval[T]]) {
  implicit val intervalOrder = (x: Interval[T], y: Interval[T]) =>
      if (x.lower == y.lower) (x.upper != y.upper && leastUpper(x.upper, y.upper) == x.upper)
      else (leastLower(x.lower, y.lower) == x.lower)

  lazy val coalesced: List[Interval[T]] = {
    val nonEmpty = intervals.filter(_.isEmpty == false)
    if (nonEmpty.size < 2) nonEmpty.toList
    else {
      val ordered = nonEmpty.toList.sortWith(intervalOrder)
      var left = ordered.head
      val iter = ordered.iterator.drop(1)
      val result = collection.mutable.LinkedHashSet[Interval[T]]()
      while (iter.hasNext) {
        val right = iter.next()
        if (left connectedTo right)
          left = Interval(leastLower(left.lower, right.lower), greatestUpper(left.upper, right.upper))
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
