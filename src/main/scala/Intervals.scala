package com.youdevise.albatross

import Bounds._

object Intervals {
  def open[T](endpoint: T)(implicit ordering: Ordering[T]) = OpenBoundBuilder[T](endpoint)
  def closed[T](endpoint: T)(implicit ordering: Ordering[T]) = ClosedBoundBuilder[T](endpoint)
  def unbounded[T]()(implicit ordering: Ordering[T]) = UnboundedBuilder[T]
}

sealed class Interval[T](val lower: MaybeLowerBound[T], val upper: MaybeUpperBound[T]) {

  val isASingleton: Boolean = (upper, lower) match {
    case (Some(a), Some(b)) => a.isClosed && b.isClosed && a.endpoint == b.endpoint
    case _ => false
  }

  def encloses(value: T): Boolean = lower.map(_.encloses(value)).getOrElse(true) && upper.map(_.encloses(value)).getOrElse(true)

  def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean =
    encloses(values.min) && encloses(values.max)

  def enclosesBound(bound: Option[Bound[T]]): Boolean = bound.map(b => encloses(b.endpoint)).getOrElse(false)

  def enclosesInterval(other: Interval[T]): Boolean =
    (leastLower(lower, other.lower) == this.lower) && (greatestUpper(upper, other.upper) == this.upper)

  def connectedTo(other: Interval[T]): Boolean =
    (this enclosesBound other.lower) || (this enclosesBound other.upper) ||
         (other enclosesBound lower) || (other enclosesBound upper)

  def intersect(other: Interval[T]): Option[Interval[T]] =
    if (!connectedTo(other)) None
    else Some(Interval(greatestLower(lower, other.lower), leastUpper(upper, other.upper)))

  def union(other: Interval[T]): Set[Interval[T]] =
    if (connectedTo(other)) Set(Interval(leastLower(lower, other.lower), greatestUpper(upper, other.upper)))
    else Set(this, other)

  def leftOf(other: Interval[T]): Boolean = (upper, other.lower) match {
      case (_, None) => false
      case (None, _) => true
      case (Some(a), Some(b)) => !b.encloses(a.endpoint)
    }    

  def complement(other: Interval[T]): Set[Interval[T]] =
    if (this == other) Set.empty[Interval[T]]
    else if (!connectedTo(other)) Set(this)
    else if (other enclosesInterval this) Set.empty[Interval[T]]
    else if (this enclosesInterval other) Set(
      Interval(lower, other.lower.map(_.inverse)),
      Interval(other.upper.map(_.inverse), upper))
    else if (this enclosesBound other.lower) Set(Interval(lower, other.lower.map(_.inverse)))
    else Set(Interval(other.upper.map(_.inverse), upper))

  override def toString: String = Interval.represent(lower, upper)

  override def equals(other: Any): Boolean = other match {
    case interval: Interval[T] => interval.lower == lower && interval.upper == upper
    case _ => false
  }

  override def hashCode: Int = (lower, upper).hashCode()
}

object Interval {

  def represent[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]): String = "%s...%s".format(lower.map(_.toString).getOrElse("∞"), upper.map(_.toString).getOrElse("∞"))

  def apply[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]): Interval[T] = {
    val isEmpty = (for {
      boundedLower <- lower
      boundedUpper <- upper
    } yield !(boundedLower.encloses(boundedUpper.endpoint) && boundedUpper.encloses(boundedLower.endpoint))).getOrElse(false)
    if (isEmpty) throw new IllegalArgumentException("An interval created with the bounds %s will be empty".format(represent(lower, upper)))
    new Interval(lower, upper)
  }

  def unapply[T](interval: Interval[T]) = Some((interval.lower, interval.upper))
}

sealed case class IntervalSet[T](intervals: Interval[T]*) {
  implicit val intervalOrder = (x: Interval[T], y: Interval[T]) =>
      if (x.lower == y.lower) (x.upper != y.upper && leastUpper(x.upper, y.upper) == x.upper)
      else (leastLower(x.lower, y.lower) == x.lower)

  lazy val coalesced: List[Interval[T]] = {
    val ordered = intervals.toList.distinct.sortWith(intervalOrder)
    if (ordered.size < 2) ordered
    else {
      def coalesce(coalesced: List[Interval[T]], uncoalesced: List[Interval[T]], left: Interval[T]): List[Interval[T]] = uncoalesced match {
        case Nil => left :: coalesced
        case right :: tail => if (left connectedTo right) 
          coalesce(coalesced, tail, Interval(leastLower(left.lower, right.lower), greatestUpper(left.upper, right.upper)))
          else coalesce(left :: coalesced, tail, right)
      }
      val reverseOrdered = coalesce(Nil, ordered.tail, ordered.head)
      reverseOrdered.reverse
    }
  }

  def encloses(value: T): Boolean = coalesced.find(_.encloses(value)).isDefined

  def enclosesAll(values: Iterable[T]): Boolean = values.forall(encloses(_))

  def union(other: IntervalSet[T]): IntervalSet[T] = IntervalSet((coalesced ++ other.coalesced):_*)
  
  def intersect(other: IntervalSet[T]): IntervalSet[T] = {
    def iterate(leftList: List[Interval[T]], rightList: List[Interval[T]], intersections: List[Interval[T]]): List[Interval[T]] = (leftList, rightList) match {
      case (Nil, _) => intersections
      case (_, Nil) => intersections
      case (l :: ls, r :: rs) =>
        println("checking %s against %s with intersections %s".format(l, r, intersections))
        if (l enclosesInterval r) iterate(leftList, rs, r :: intersections)
        else if (r enclosesInterval l) iterate(ls, rightList, l :: intersections)
        else if (l connectedTo r)
          if (l enclosesBound r.lower) iterate(ls, rightList, (l intersect r).get :: intersections)
          else iterate(leftList, rs, (l intersect r).get :: intersections)
        else if (l leftOf r) iterate(ls, rightList, intersections)
        else iterate(leftList, rs, intersections)
    }
    IntervalSet(iterate(this.coalesced, other.coalesced, Nil):_*)
  }

  def complement(other: IntervalSet[T]): IntervalSet[T] = {
    val complements = coalesced.flatMap { interval =>
      other.coalesced.foldLeft(Set(interval)) { (acc, other) =>
        acc.flatMap(_ complement other)
      }
    }
    IntervalSet(complements:_*)
  }
  
  override def toString: String = coalesced.map(_.toString).mkString(" ∪ ")

}
