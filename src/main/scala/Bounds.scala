package com.youdevise.albatross

trait Bound[T] {
  val ordering: Ordering[T]
  val isBounded: Boolean
  val isUnbounded: Boolean = !isBounded
  val isOpen: Boolean
  val isClosed: Boolean = !isOpen
  val isUpper: Boolean
  val isLower: Boolean = !isUpper

  def outsideTest(endpoint: T, value: T): Boolean
  def boundaryTest(value: T): Boolean
  def encloses(value: T): Boolean
  
  def map[T2](f: T => T2)(implicit ordering: Ordering[T2]): Bound[T2]
  def flatMap[T2](f: T => Bound[T2])(implicit ordering: Ordering[T2]): Bound[T2]
  def filter(f: T => Boolean): Boolean
}

trait Unbounded[T] { self: Bound[T] =>
  override val isBounded = false
  override val isOpen = true
  override def outsideTest(endpoint: T, value: T): Boolean = true
  override def boundaryTest(value: T): Boolean = true
  override def encloses(value: T): Boolean = true
  override def filter(f: T => Boolean): Boolean = false
}

trait Bounded[T] { self: Bound[T] =>
  val endpoint: T
  override val isBounded = true
  override def encloses(value: T): Boolean = outsideTest(endpoint, value) && boundaryTest(value)
  override def flatMap[T2](f: T => Bound[T2])(implicit ordering: Ordering[T2]): Bound[T2] = f(endpoint)
  override def filter(f: T => Boolean): Boolean = f(endpoint)
}

object Bounded {
  def unapply[T](bounded: Bounded[T]): Option[T] = Some(bounded.endpoint)
}

trait UpperBound[T] { self: Bound[T] =>
  override val isUpper = true
  override def outsideTest(endpoint: T, value: T): Boolean = ordering.gteq(endpoint, value)

  def innerMost(other: Bound[T] with UpperBound[T]): Bound[T] with UpperBound[T] = other match {
    case bounded: Bounded[T] if !encloses(bounded.endpoint) => this
    case _ => other
  }

  def outerMost(other: Bound[T] with UpperBound[T]): Bound[T] with UpperBound[T] = other match {
    case bounded: Bounded[T] if encloses(bounded.endpoint) => this
    case _ => other
  }

  val inverse: Bound[T] with LowerBound[T]
}

trait LowerBound[T] { self: Bound[T] =>
  override val isUpper = false
  override def outsideTest(endpoint: T, value: T): Boolean = ordering.lteq(endpoint, value)

  def innerMost(other: Bound[T] with LowerBound[T]): Bound[T] with LowerBound[T] = other match {
    case Bounded(endpoint: T) if !encloses(bounded.endpoint) => this
    case _ => other
  }

  def outerMost(other: Bound[T] with LowerBound[T]): Bound[T] with LowerBound[T] = other match {
    case Bounded(endpoint: T) if encloses(bounded.endpoint) => this
    case _ => other
  }

  val inverse: Bound[T] with UpperBound[T]
}

trait ClosedBound[T] extends Bounded[T] { self: Bound[T] =>
  override val isOpen = false
  override val isClosed = true
  override def boundaryTest(value: T) = true
}

trait OpenBound[T] extends Bounded[T] { self: Bound[T] =>
  override val isOpen = true
  override val isClosed = false
  override def boundaryTest(value: T) = ordering.compare(value, endpoint) != 0
}

sealed case class UnboundedUpper[T]()(implicit val ordering: Ordering[T]) extends Bound[T] with Unbounded[T] with UpperBound[T] {
  def map[T2](f: T => T2)(implicit ordering: Ordering[T2]) = UnboundedUpper[T2]
  def flatMap[T2](f: T => Bound[T2])(implicit ordering: Ordering[T2]): Bound[T2] = UnboundedUpper[T2]
  override lazy val inverse: Bound[T] with LowerBound[T] = throw new UnsupportedOperationException("Cannot invert an unbounded bound")
}

sealed case class UnboundedLower[T]()(implicit val ordering: Ordering[T]) extends Bound[T] with Unbounded[T] with LowerBound[T] {
  def map[T2](f: T => T2)(implicit ordering: Ordering[T2]) = UnboundedLower[T2]
  def flatMap[T2](f: T => Bound[T2])(implicit ordering: Ordering[T2]): Bound[T2] = UnboundedLower[T2]
  override lazy val inverse: Bound[T] with UpperBound[T] = throw new UnsupportedOperationException("Cannot invert an unbounded bound")
}

sealed case class OpenUpperBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends Bound[T] with UpperBound[T] with OpenBound[T] {
  def map[T2](f: T => T2)(implicit ordering: Ordering[T2]) = OpenUpperBound(f(endpoint))
  override lazy val inverse: Bound[T] with LowerBound[T] = ClosedLowerBound(endpoint)
}

sealed case class ClosedUpperBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends Bound[T] with UpperBound[T] with ClosedBound[T] {
  def map[T2](f: T => T2)(implicit ordering: Ordering[T2]) = ClosedUpperBound(f(endpoint))
  override lazy val inverse: Bound[T] with LowerBound[T] = OpenLowerBound(endpoint)
}

sealed case class OpenLowerBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends Bound[T] with LowerBound[T] with OpenBound[T] {
  def map[T2](f: T => T2)(implicit ordering: Ordering[T2]) = OpenLowerBound(f(endpoint))
  override lazy val inverse: Bound[T] with UpperBound[T] = ClosedUpperBound(endpoint)
}

sealed case class ClosedLowerBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends Bound[T] with LowerBound[T] with ClosedBound[T] {
  def map[T2](f: T => T2)(implicit ordering: Ordering[T2]) = ClosedLowerBound(f(endpoint))
  override lazy val inverse: Bound[T] with UpperBound[T] = OpenUpperBound(endpoint)
}