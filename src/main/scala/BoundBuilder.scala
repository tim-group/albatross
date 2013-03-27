package com.youdevise.albatross

trait BoundBuilder[T] {
  def buildLower: Bound[T] with LowerBound[T]
  def buildUpper: Bound[T] with UpperBound[T]
  def to(upperBoundBuilder: BoundBuilder[T]) = Interval(buildLower, upperBoundBuilder.buildUpper)
}

sealed case class OpenBoundBuilder[T](endpoint: T)(implicit ordering: Ordering[T]) extends BoundBuilder[T] {
  def buildLower = OpenLowerBound(endpoint)
  def buildUpper = OpenUpperBound(endpoint)
}

sealed case class ClosedBoundBuilder[T](endpoint: T)(implicit ordering: Ordering[T]) extends BoundBuilder[T] {
  def buildLower = ClosedLowerBound(endpoint)
  def buildUpper = ClosedUpperBound(endpoint)
}

sealed case class UnboundedBuilder[T]()(implicit ordering: Ordering[T]) extends BoundBuilder[T] {
  def buildLower = UnboundedLower[T]
  def buildUpper = UnboundedUpper[T]
}
