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

  def intersect(other: Interval[T]): IntervalSet[T] =
    if (!connectedTo(other)) IntervalSet()
    else IntervalSet(Interval(greatestLower(lower, other.lower), leastUpper(upper, other.upper)))
    
  def intersect(other: IntervalSet[T]): IntervalSet[T] = other.intersect(this)

  def union(other: Interval[T]): IntervalSet[T] =
    if (connectedTo(other)) IntervalSet(Interval(leastLower(lower, other.lower), greatestUpper(upper, other.upper)))
    else IntervalSet(this, other)
    
  def union(other: IntervalSet[T]): IntervalSet[T] = other.union(this)

  def complement(other: Interval[T]): IntervalSet[T] =
    if (this == other) IntervalSet[T]()
    else if (other enclosesInterval this) IntervalSet[T]()
    else if (!connectedTo(other)) IntervalSet(this)    
    else if (this enclosesInterval other) IntervalSet(
      Interval(lower, other.lower.map(_.inverse)),
      Interval(other.upper.map(_.inverse), upper))
    else if (this enclosesBound other.lower) IntervalSet(Interval(lower, other.lower.map(_.inverse)))
    else IntervalSet(Interval(other.upper.map(_.inverse), upper))

  override def toString: String = Interval.represent(lower, upper)

  override def equals(other: Any): Boolean = other match {
    case Interval(otherLower, otherUpper) => otherLower == lower && otherUpper == upper
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

  def unapply(interval: Interval[_]) = Some((interval.lower, interval.upper))
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

  def union(other: Interval[T]): IntervalSet[T] = IntervalSet((other :: intervals.toList):_*)
  def union(other: IntervalSet[T]): IntervalSet[T] = IntervalSet((coalesced ++ other.coalesced):_*)
  
  def intersect(other: Interval[T]): IntervalSet[T] = IntervalSet((coalesced.flatMap(self => (self intersect other).intervals)):_*)
  def intersect(other: IntervalSet[T]): IntervalSet[T] = {
    def addIntersection(l: Interval[T], r: Interval[T], intersections: List[Interval[T]]) =
      (l intersect r).intervals.headOption.map(_ :: intersections).getOrElse(intersections)
      
    def iterate(leftList: List[Interval[T]], rightList: List[Interval[T]], intersections: List[Interval[T]]): List[Interval[T]] =
      (leftList, rightList) match {
        case (Nil, _) => intersections
        case (_, Nil) => intersections
        case (l :: ls, r :: rs) =>        
          if (l enclosesInterval r) iterate(leftList, rs, r :: intersections)
          else if (r enclosesInterval l) iterate(ls, rightList, l :: intersections)
          else if (intervalOrder(l, r)) iterate(ls, rightList, addIntersection(l, r, intersections))
          else iterate(leftList, rs, addIntersection(l, r, intersections))
      }    
    
    IntervalSet(iterate(this.coalesced, other.coalesced, Nil):_*)
  }

  def complement(other: IntervalSet[T]): IntervalSet[T] = {
    val complements = coalesced.flatMap { interval =>
      other.coalesced.foldLeft(Set(interval)) { (acc, other) =>
        acc.flatMap(self => (self complement other).intervals.toSet)
      }
    }
    IntervalSet(complements:_*)
  }
  
  override def toString: String = coalesced.map(_.toString).mkString(" ∪ ")

}
