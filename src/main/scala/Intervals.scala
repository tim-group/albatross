package com.youdevise.albatross

import Bounds._

object Intervals {
  def open[T](endpoint: T)(implicit ordering: Ordering[T]) = OpenBoundBuilder[T](endpoint)
  def closed[T](endpoint: T)(implicit ordering: Ordering[T]) = ClosedBoundBuilder[T](endpoint)
  def unbounded[T]()(implicit ordering: Ordering[T]) = UnboundedBuilder[T]
}

trait IntervalSet[T] extends Function[T, Boolean] {
  def isEmpty: Boolean
  def isContinuous: Boolean
  def isASingleton: Boolean
  def subIntervals: List[SubInterval[T]]

  def encloses(value: T): Boolean
  override def apply(value: T): Boolean = encloses(value)
  def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean

  def enclosesInterval(other: IntervalSet[T]): Boolean
  def connectedTo(other: IntervalSet[T]): Boolean
  def intersect(other: IntervalSet[T]): IntervalSet[T]
  def union(other: IntervalSet[T]): IntervalSet[T]
  def complement(other: IntervalSet[T]): IntervalSet[T]
}

trait NonEmpty[T] extends IntervalSet[T] {
  override def isEmpty = false
  val lower: MaybeLowerBound[T]
  val upper: MaybeUpperBound[T]
}

trait Continuous[T] extends IntervalSet[T] {
  override def isContinuous = true
}

trait SubInterval[T] extends NonEmpty[T] with Continuous[T] {}

object IntervalSet {

  type SubIntervals[T] = List[SubInterval[T]]
  
  implicit def intervalOrder[T] = (x: NonEmpty[T], y: NonEmpty[T]) =>
    if (x.lower == y.lower) (x.upper != y.upper && leastUpper(x.upper, y.upper) == x.upper)
    else (leastLower(x.lower, y.lower) == x.lower)

  def empty[T]: Continuous[T] = EmptyInterval[T]()

  def apply[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]): Continuous[T] = {
    val isEmpty = (for {
      boundedLower <- lower
      boundedUpper <- upper
    } yield !(boundedLower.encloses(boundedUpper.endpoint) && boundedUpper.encloses(boundedLower.endpoint)))
    .getOrElse(false)
    
    if (isEmpty) empty[T]
    else NonEmptyContinuousInterval(lower, upper)
  }

  def apply[T](intervalSets: IntervalSet[T]*): IntervalSet[T] = apply(intervalSets.toList)
  def apply[T](intervalSets: Iterable[IntervalSet[T]]): IntervalSet[T] = {
    val nonEmpty = intervalSets.toList.flatMap(_.subIntervals).distinct
    if (nonEmpty.isEmpty) empty[T]
    else {
      val coalesced = coalesce(nonEmpty)
      if (coalesced.length == 1) coalesced.head
      else DiscontinuousIntervalSet(coalesced)
    }
  }

  private def coalesce[T](intervals: SubIntervals[T]): SubIntervals[T] = {
    val ordered = intervals.sortWith(intervalOrder[T])
    if (ordered.size < 2) ordered
    else {
      def coalesce(coalesced: SubIntervals[T],
                   uncoalesced: SubIntervals[T],
                   left: SubInterval[T]): SubIntervals[T] =
        uncoalesced match {
          case Nil => left :: coalesced
          case right :: tail => if (left connectedTo right)
            coalesce(coalesced, tail, NonEmptyContinuousInterval(leastLower(left.lower, right.lower), greatestUpper(left.upper, right.upper)))
            else coalesce(left :: coalesced, tail, right)
        }
      
      coalesce(Nil, ordered.tail, ordered.head).reverse
    }
  }

  private sealed case class EmptyInterval[T]() extends IntervalSet[T] with Continuous[T] {
    override def isEmpty = true
    override def isASingleton = false
    override def subIntervals: SubIntervals[T] = List.empty

    override def encloses(value: T): Boolean = false
    override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean = false
    override def enclosesInterval(other: IntervalSet[T]): Boolean = false
    override def connectedTo(other: IntervalSet[T]): Boolean = false
    override def intersect(other: IntervalSet[T]): IntervalSet[T] = this
    override def union(other: IntervalSet[T]): IntervalSet[T] = other
    override def complement(other: IntervalSet[T]): IntervalSet[T] = this

    override def toString: String = "()"
  }

  private sealed case class NonEmptyContinuousInterval[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T])
    extends SubInterval[T] {

    override val isASingleton: Boolean = (upper, lower) match {
      case (Some(a), Some(b)) => a.isClosed && b.isClosed && a.endpoint == b.endpoint
      case _ => false
    }

    override def subIntervals: SubIntervals[T] = List(this)

    override def encloses(value: T): Boolean =
      lower.map(_.encloses(value)).getOrElse(true) && upper.map(_.encloses(value)).getOrElse(true)

    override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean =
      encloses(values.min) && encloses(values.max)

    override def enclosesInterval(other: IntervalSet[T]): Boolean =
      other.subIntervals.forall(nec =>
        (leastLower(lower, nec.lower) == this.lower) && (greatestUpper(upper, nec.upper) == this.upper))

    private[this] def enclosesBound(intervalSet: IntervalSet[T], bound: Option[Bound[T]]): Boolean =
      bound.map(intervalSet encloses _.endpoint).getOrElse(false)

    override def connectedTo(other: IntervalSet[T]): Boolean =
      other.subIntervals.find(nec =>
        (enclosesBound(this, nec.lower)) || (enclosesBound(this, nec.upper)) ||
          (enclosesBound(nec, lower)) || (enclosesBound(nec, upper))).isDefined

    override def intersect(other: IntervalSet[T]): IntervalSet[T] =
      IntervalSet(other.subIntervals.map(nec =>
        IntervalSet(greatestLower(lower, nec.lower), leastUpper(upper, nec.upper))).flatMap(_.subIntervals))

    override def union(other: IntervalSet[T]): IntervalSet[T] = IntervalSet(this :: other.subIntervals)

    private def complementNonEmptyContinuous(other: SubInterval[T]): IntervalSet[T] =
      if (this == other) IntervalSet.empty
      else if (other enclosesInterval this) IntervalSet.empty
      else if (!connectedTo(other)) this
      else if (this enclosesInterval other) IntervalSet(
        NonEmptyContinuousInterval(lower, other.lower.map(_.inverse)),
        NonEmptyContinuousInterval(other.upper.map(_.inverse), upper))
      else if (enclosesBound(this, other.lower)) NonEmptyContinuousInterval(lower, other.lower.map(_.inverse))
      else NonEmptyContinuousInterval(other.upper.map(_.inverse), upper)

    override def complement(other: IntervalSet[T]): IntervalSet[T] =
      IntervalSet(other.subIntervals.foldLeft(List(this)) { (acc, nec) =>
        acc.flatMap(_.complementNonEmptyContinuous(nec).subIntervals.map(_.asInstanceOf[NonEmptyContinuousInterval[T]]))
      })

    override def toString: String = "%s...%s".format(lower.map(_.toString).getOrElse("\u221e"), upper.map(_.toString).getOrElse("\u221e"))
  }

  private sealed case class DiscontinuousIntervalSet[T](subIntervals: SubIntervals[T])
    extends IntervalSet[T] with NonEmpty[T] {
    override def isASingleton = false
    override val isContinuous: Boolean = false

    override val lower = subIntervals.head.lower
    override val upper = subIntervals.last.upper

    override def encloses(value: T): Boolean = subIntervals.find(_.encloses(value)).isDefined
    override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean = values.forall(encloses(_))

    override def enclosesInterval(other: IntervalSet[T]): Boolean =
      (this intersect other) == other

    override def connectedTo(other: IntervalSet[T]): Boolean =
      other.subIntervals.find(otherSubInterval =>
        subIntervals.find(_.connectedTo(otherSubInterval)).isDefined
      ).isDefined

    override def union(other: IntervalSet[T]): IntervalSet[T] = IntervalSet(subIntervals ++ other.subIntervals)

    override def intersect(other: IntervalSet[T]): IntervalSet[T] = {
      def addIntersection(l: SubInterval[T],
                          r: SubInterval[T],
                          intersections: SubIntervals[T]) =
        (l intersect r).subIntervals.headOption.map(_ :: intersections).getOrElse(intersections)

      def iterate(leftList: SubIntervals[T],
                  rightList: SubIntervals[T],
                  intersections: SubIntervals[T]): SubIntervals[T] =
        (leftList, rightList) match {
          case (Nil, _) => intersections
          case (_, Nil) => intersections
          case (l :: ls, r :: rs) =>
            if (l enclosesInterval r) iterate(leftList, rs, r :: intersections)
            else if (r enclosesInterval l) iterate(ls, rightList, l :: intersections)
            else if (intervalOrder(l, r)) iterate(ls, rightList, addIntersection(l, r, intersections))
            else iterate(leftList, rs, addIntersection(l, r, intersections))
        }

      IntervalSet(iterate(subIntervals, other.subIntervals, Nil))
    }

    override def complement(other: IntervalSet[T]): IntervalSet[T] = {
      val complements = subIntervals.flatMap { interval =>
        other.subIntervals.foldLeft(List(interval)) { (acc, otherC) =>
          acc.flatMap(a => (a complement otherC).subIntervals)
        }
      }
      IntervalSet(complements)
    }

    override def toString: String = subIntervals.map(_.toString).mkString(" \u221a ")

  }
}