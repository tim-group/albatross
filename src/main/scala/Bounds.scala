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
}

trait Upper[T] { self: Bound[T] =>
  override val isUpper = true
  override def outsideTest(value: T): Boolean = ordering.gteq(endpoint, value)

  val inverse: Bound[T] with Lower[T]
}

trait Lower[T] { self: Bound[T] =>
  override val isUpper = false
  override def outsideTest(value: T): Boolean = ordering.lteq(endpoint, value)

  val inverse: Bound[T] with Upper[T]
}

trait ClosedBound[T] extends Bound[T] {
  override val isOpen = false
  override def boundaryTest(value: T) = true
}

trait OpenBound[T] extends Bound[T] {
  override val isOpen = true
  override def boundaryTest(value: T) = ordering.compare(value, endpoint) != 0
}

sealed case class OpenUpperBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends OpenBound[T] with Upper[T] {
  override lazy val inverse: Bound[T] with Lower[T] = ClosedLowerBound(endpoint)
  override def toString: String = "%s)".format(endpoint)
}

sealed case class ClosedUpperBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends ClosedBound[T] with Upper[T] {
  override lazy val inverse: Bound[T] with Lower[T] = OpenLowerBound(endpoint)
  override def toString: String = "%s]".format(endpoint)
}

sealed case class OpenLowerBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends OpenBound[T] with Lower[T] {
  override lazy val inverse: Bound[T] with Upper[T] = ClosedUpperBound(endpoint)
  override def toString: String = "(%s".format(endpoint)
}

sealed case class ClosedLowerBound[T](endpoint: T)(implicit val ordering: Ordering[T]) extends ClosedBound[T] with Lower[T] {
  override lazy val inverse: Bound[T] with Upper[T] = OpenUpperBound(endpoint)
  override def toString: String = "[%s".format(endpoint)
}

object Bounds {

  type UpperBound[T] = Bound[T] with Upper[T]
  type LowerBound[T] = Bound[T] with Lower[T]
  type MaybeUpperBound[T] = Option[UpperBound[T]]
  type MaybeLowerBound[T] = Option[LowerBound[T]]

  def innerMost[T, B <: Bound[T]](a: Option[B], b: Option[B]): Option[B] =
  (a, b) match {
    case (Some(boundedA), Some(boundedB)) => if (boundedA encloses boundedB.endpoint) Some(boundedB) else Some(boundedA)
    case (Some(_), None) => a
    case (None, Some(_))    => b
    case (None, None)    => None
  }

  def outerMost[T, B <: Bound[T]](a: Option[B], b: Option[B]): Option[B] =
    (a, b) match {
      case (Some(boundedA), Some(boundedB)) => if (boundedA encloses boundedB.endpoint) Some(boundedA) else Some(boundedB)
      case (Some(_), None) => b
      case (None, Some(_)) => a
      case (None, None)    => None
    }

  def greatestLower[T](a: MaybeLowerBound[T], b: MaybeLowerBound[T]): MaybeLowerBound[T] = innerMost[T, LowerBound[T]](a, b)
  def leastUpper[T](a: MaybeUpperBound[T], b: MaybeUpperBound[T]): MaybeUpperBound[T] = innerMost[T, UpperBound[T]](a, b)
  def leastLower[T](a: MaybeLowerBound[T], b: MaybeLowerBound[T]): MaybeLowerBound[T] = outerMost[T,  LowerBound[T]](a, b)
  def greatestUpper[T](a: MaybeUpperBound[T], b: MaybeUpperBound[T]): MaybeUpperBound[T] = outerMost[T, UpperBound[T]](a, b)
  
  def open[T](endpoint: T)(implicit ordering: Ordering[T]): OpenBoundBuilder[T] = OpenBoundBuilder[T](endpoint)
  def closed[T](endpoint: T)(implicit ordering: Ordering[T]): ClosedBoundBuilder[T] = ClosedBoundBuilder[T](endpoint)
  def unbounded[T]()(implicit ordering: Ordering[T]): UnboundedBuilder[T] = UnboundedBuilder[T]
}