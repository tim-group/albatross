package com.youdevise.albatross

trait BoundBuilder[T] {
  def buildLower: Option[Bound[T] with LowerBound[T]]
  def buildUpper: Option[Bound[T] with UpperBound[T]]
  def to(upperBoundBuilder: BoundBuilder[T]) = Interval(buildLower, upperBoundBuilder.buildUpper)
}

sealed case class OpenBoundBuilder[T](endpoint: T)(implicit ordering: Ordering[T]) extends BoundBuilder[T] {
  def buildLower = Some(OpenLowerBound(endpoint))
  def buildUpper = Some(OpenUpperBound(endpoint))
}

sealed case class ClosedBoundBuilder[T](endpoint: T)(implicit ordering: Ordering[T]) extends BoundBuilder[T] {
  def buildLower = Some(ClosedLowerBound(endpoint))
  def buildUpper = Some(ClosedUpperBound(endpoint))
}

sealed case class UnboundedBuilder[T]()(implicit ordering: Ordering[T]) extends BoundBuilder[T] {
  def buildLower = None
  def buildUpper = None
}
