package com.youdevise.albatross

import Bounds._

trait BoundBuilder[T] {
  def buildLower: Option[Bound[T] with Lower[T]]
  def buildUpper: Option[Bound[T] with Upper[T]]
  def to(upperBoundBuilder: BoundBuilder[T]): Continuous[T] = IntervalSet(buildLower, upperBoundBuilder.buildUpper)
  def toOpen(endpoint: T)(implicit ord: Ordering[T]): Continuous[T] = to(open(endpoint))
  def toClosed(endpoint: T)(implicit ord: Ordering[T]): Continuous[T] = to(closed(endpoint))
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
