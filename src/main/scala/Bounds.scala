package com.youdevise.albatross

trait Bound[T] {
  val endpoint: T
  val ordering: Ordering[T]

  val isOpen: Boolean
  val isClosed: Boolean = !isOpen

  val isUpper: Boolean
  val isLower: Boolean = !isUpper

  def outsideTest(value: T): Boolean
  def boundaryTest(value: T): Boolean

  def encloses(value: T): Boolean = outsideTest(value) && boundaryTest(value)

  def innerMost(other: Bound[T]): Bound[T] =
    if (!encloses(other.endpoint)) this else other

  def outerMost(other: Bound[T]): Bound[T] =
    if (encloses(other.endpoint)) this else other
}

trait UpperBound[T] { self: Bound[T] =>
  override val isUpper = true
  override def outsideTest(value: T): Boolean = ordering.gteq(endpoint, value)

  val inverse: Bound[T] with LowerBound[T]
}

trait LowerBound[T] { self: Bound[T] =>
  override val isUpper = false
  override def outsideTest(value: T): Boolean = ordering.lteq(endpoint, value)

  val inverse: Bound[T] with UpperBound[T]
}

trait ClosedBound[T] extends Bound[T] {
  override val isOpen = false
  override def boundaryTest(value: T) = true
}

trait OpenBound[T] extends Bound[T] {
  override val isOpen = true
  override def boundaryTest(value: T) = ordering.compare(value, endpoint) != 0
}

sealed case class OpenUpperBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends OpenBound[T] with UpperBound[T] {
  override lazy val inverse: Bound[T] with LowerBound[T] = ClosedLowerBound(endpoint)
}

sealed case class ClosedUpperBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends ClosedBound[T] with UpperBound[T] {
  override lazy val inverse: Bound[T] with LowerBound[T] = OpenLowerBound(endpoint)
}

sealed case class OpenLowerBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends OpenBound[T] with LowerBound[T] {
  override lazy val inverse: Bound[T] with UpperBound[T] = ClosedUpperBound(endpoint)
}

sealed case class ClosedLowerBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends ClosedBound[T] with LowerBound[T] {
  override lazy val inverse: Bound[T] with UpperBound[T] = OpenUpperBound(endpoint)
}

object Bounds {
  def innerMostLower[T](a: Option[Bound[T] with LowerBound[T]], b: Option[Bound[T] with LowerBound[T]]): Option[Bound[T] with LowerBound[T]] =
    for { boundedA <- a; boundedB <- b } yield boundedA.innerMost(boundedB).asInstanceOf[Bound[T] with LowerBound[T]]

  def innerMostUpper[T](a: Option[Bound[T] with UpperBound[T]], b: Option[Bound[T] with UpperBound[T]]): Option[Bound[T] with UpperBound[T]] =
    for { boundedA <- a; boundedB <- b } yield boundedA.innerMost(boundedB).asInstanceOf[Bound[T] with UpperBound[T]]

  def outerMostLower[T](a: Option[Bound[T] with LowerBound[T]], b: Option[Bound[T] with LowerBound[T]]): Option[Bound[T] with LowerBound[T]] =
    for { boundedA <- a; boundedB <- b } yield boundedA.outerMost(boundedB).asInstanceOf[Bound[T] with LowerBound[T]]

  def outerMostUpper[T](a: Option[Bound[T] with UpperBound[T]], b: Option[Bound[T] with UpperBound[T]]): Option[Bound[T] with UpperBound[T]] =
    for { boundedA <- a; boundedB <- b } yield boundedA.outerMost(boundedB).asInstanceOf[Bound[T] with UpperBound[T]]
}