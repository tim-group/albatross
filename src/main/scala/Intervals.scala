package com.youdevise.albatross

import Bounds._

object Intervals {
  def open[T](endpoint: T)(implicit ordering: Ordering[T]) = OpenBoundBuilder[T](endpoint)
  def closed[T](endpoint: T)(implicit ordering: Ordering[T]) = ClosedBoundBuilder[T](endpoint)
  def unbounded[T]()(implicit ordering: Ordering[T]) = UnboundedBuilder[T]
}

trait IntervalSet[T] {
  def isEmpty: Boolean
  def isContinuous: Boolean
  def isASingleton: Boolean
  def nonEmptyContinuousSubIntervals: List[NonEmptyContinuousIntervalSet[T]]
  def nonEmptyOption: Option[NonEmpty[T]]
  def nonEmpty = nonEmptyOption.get
  
  def encloses(value: T): Boolean
  def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean
  def enclosesBound(bound: Option[Bound[T]]): Boolean
  def enclosesInterval(other: IntervalSet[T]): Boolean
  def connectedTo(other: IntervalSet[T]): Boolean
  def intersect(other: IntervalSet[T]): IntervalSet[T]
  def union(other: IntervalSet[T]): IntervalSet[T]
  def complement(other: IntervalSet[T]): IntervalSet[T]
}

trait NonEmpty[T] extends IntervalSet[T] {
  override def isEmpty = false
  override def nonEmptyOption = Some(this)
  val lower: MaybeLowerBound[T]
  val upper: MaybeUpperBound[T]
}

trait Continuous[T] extends IntervalSet[T] {
  override def isContinuous = true
}

trait NonEmptyContinuousIntervalSet[T] extends NonEmpty[T] with Continuous[T] { }

object IntervalSet {
      
  implicit def intervalOrder[T] = (x: NonEmptyContinuousIntervalSet[T], y: NonEmptyContinuousIntervalSet[T]) =>
      if (x.lower == y.lower) (x.upper != y.upper && leastUpper(x.upper, y.upper) == x.upper)
      else (leastLower(x.lower, y.lower) == x.lower)  

  def empty[T]: Continuous[T] = EmptyInterval[T]()

  def apply[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]): Continuous[T] = {
    val isEmpty = (for {
      boundedLower <- lower
      boundedUpper <- upper
    } yield !(boundedLower.encloses(boundedUpper.endpoint) && boundedUpper.encloses(boundedLower.endpoint))).getOrElse(false)
    if (isEmpty) empty[T]
    else NonEmptyContinuousInterval(lower, upper)
  }
  
  def apply[T](continuousInterval: Continuous[T]): Continuous[T] = continuousInterval
  def apply[T](continuousIntervals: Continuous[T]*): IntervalSet[T] = apply(continuousIntervals.toSeq)
  def apply[T](continuousIntervals: Iterable[Continuous[T]]): IntervalSet[T] = {
    val nonEmpty = continuousIntervals.filter(!_.isEmpty).map(_.asInstanceOf[NonEmptyContinuousIntervalSet[T]])
    if (nonEmpty.isEmpty) empty[T]
    else {
      val coalesced = coalesce(nonEmpty)
      if (coalesced.length == 1) coalesced.head
      else DiscontinuousIntervalSet(coalesced)
    }
  }

  def unapply(interval: NonEmpty[_]) = Some((interval.lower, interval.upper))

  private def coalesce[T](intervals: Iterable[NonEmptyContinuousIntervalSet[T]]): List[NonEmptyContinuousIntervalSet[T]] = {
    val ordered = intervals.toList.distinct.sortWith(intervalOrder[T])
    if (ordered.size < 2) ordered
    else {
      def coalesce(coalesced: List[NonEmptyContinuousIntervalSet[T]], uncoalesced: List[NonEmptyContinuousIntervalSet[T]], left: NonEmptyContinuousIntervalSet[T]): List[NonEmptyContinuousIntervalSet[T]] = uncoalesced match {
        case Nil => left :: coalesced
        case right :: tail => if (left connectedTo right) 
          coalesce(coalesced, tail, NonEmptyContinuousInterval(leastLower(left.lower, right.lower), greatestUpper(left.upper, right.upper)))
          else coalesce(left :: coalesced, tail, right)
      }
      val reverseOrdered = coalesce(Nil, ordered.tail, ordered.head)
      reverseOrdered.reverse
    }
  }
}

sealed case class EmptyInterval[T]() extends IntervalSet[T] with Continuous[T] {
  override def isEmpty = true
  override def nonEmptyOption = None
  override def isASingleton = false
  override def nonEmptyContinuousSubIntervals: List[NonEmptyContinuousIntervalSet[T]] = List.empty
  override def encloses(value: T): Boolean = false
  override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean = false
  override def enclosesBound(bound: Option[Bound[T]]): Boolean = false
  override def enclosesInterval(other: IntervalSet[T]): Boolean = false
  override def connectedTo(other: IntervalSet[T]): Boolean = false
  override def intersect(other: IntervalSet[T]): IntervalSet[T] = this
  override def union(other: IntervalSet[T]): IntervalSet[T] = other
  override def complement(other: IntervalSet[T]): IntervalSet[T] = this

  override def toString: String = "()"
}

sealed case class NonEmptyContinuousInterval[T] private[albatross](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T])
  extends NonEmptyContinuousIntervalSet[T] {
  
  override val isASingleton: Boolean = (upper, lower) match {
    case (Some(a), Some(b)) => a.isClosed && b.isClosed && a.endpoint == b.endpoint
    case _ => false
  }
  
  override def nonEmptyContinuousSubIntervals: List[NonEmptyContinuousIntervalSet[T]] = List(this)

  override def encloses(value: T): Boolean = lower.map(_.encloses(value)).getOrElse(true) && upper.map(_.encloses(value)).getOrElse(true)

  override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean =
    encloses(values.min) && encloses(values.max)

  override def enclosesBound(bound: Option[Bound[T]]): Boolean = bound.map(b => encloses(b.endpoint)).getOrElse(false)

  private[this] def enclosesNonEmptyContinuousInterval(other: NonEmptyContinuousIntervalSet[T]): Boolean =
    (leastLower(lower, other.lower) == this.lower) && (greatestUpper(upper, other.upper) == this.upper)
    
  override def enclosesInterval(other: IntervalSet[T]): Boolean =
    other.nonEmptyContinuousSubIntervals.forall(enclosesNonEmptyContinuousInterval(_))

  private[this] def connectedToNonEmptyContinuous(other: NonEmptyContinuousIntervalSet[T]): Boolean =
    (this enclosesBound other.lower) || (this enclosesBound other.upper) ||
         (other enclosesBound lower) || (other enclosesBound upper)
         
  override def connectedTo(other: IntervalSet[T]): Boolean =
    other.nonEmptyContinuousSubIntervals.find(connectedToNonEmptyContinuous(_)).isDefined

  private[this] def intersectNonEmptyContinuous(other: NonEmptyContinuousIntervalSet[T]): IntervalSet[T] =
    if (!connectedTo(other)) IntervalSet(Iterable.empty)
    else NonEmptyContinuousInterval(greatestLower(lower, other.lower), leastUpper(upper, other.upper))
    
  override def intersect(other: IntervalSet[T]): IntervalSet[T] =
    IntervalSet(other.nonEmptyContinuousSubIntervals.map(this intersectNonEmptyContinuous _).flatMap(_.nonEmptyContinuousSubIntervals))

  private[this] def unionNonEmptyContinuous(other: NonEmptyContinuousIntervalSet[T]): IntervalSet[T] =
    if (connectedTo(other)) NonEmptyContinuousInterval(leastLower(lower, other.lower), greatestUpper(upper, other.upper))
    else IntervalSet(Seq(this, other))
    
  override def union(other: IntervalSet[T]): IntervalSet[T] =
    if (other.isEmpty) this
    else if (other.isContinuous) this unionNonEmptyContinuous other.nonEmptyContinuousSubIntervals.head
    else other union this

  private[this] def complement(other: NonEmptyContinuousIntervalSet[T]): IntervalSet[T] =
    if (this == other) IntervalSet.empty
    else if (other enclosesInterval this) IntervalSet.empty
    else if (!connectedTo(other)) this 
    else if (this enclosesInterval other) IntervalSet(
      NonEmptyContinuousInterval(lower, other.lower.map(_.inverse)),
      NonEmptyContinuousInterval(other.upper.map(_.inverse), upper))
    else if (this enclosesBound other.lower) NonEmptyContinuousInterval(lower, other.lower.map(_.inverse))
    else NonEmptyContinuousInterval(other.upper.map(_.inverse), upper)
    
  override def complement(other: IntervalSet[T]): IntervalSet[T] =
    other.nonEmptyContinuousSubIntervals.foldLeft(IntervalSet(this).asInstanceOf[IntervalSet[T]])(_ complement _)

  override def toString: String = NonEmptyContinuousInterval.represent(lower, upper)
}

object NonEmptyContinuousInterval {
  def represent[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]): String = "%s...%s".format(lower.map(_.toString).getOrElse("∞"), upper.map(_.toString).getOrElse("∞"))
}

private[albatross] sealed case class DiscontinuousIntervalSet[T](nonEmptyContinuousSubIntervals: List[NonEmptyContinuousIntervalSet[T]])
  extends IntervalSet[T] with NonEmpty[T] {
  override def isASingleton = false
  override val isContinuous: Boolean = false

  override val lower = nonEmptyContinuousSubIntervals.head.lower
  override val upper = nonEmptyContinuousSubIntervals.last.upper
  
  override def encloses(value: T): Boolean = nonEmptyContinuousSubIntervals.find(_.encloses(value)).isDefined
  override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean = values.forall(encloses(_))

  override def enclosesInterval(other: IntervalSet[T]): Boolean =
    (this intersect other) == other
    
  override def enclosesBound(other: Option[Bound[T]]): Boolean = nonEmptyContinuousSubIntervals.find(_.enclosesBound(other)).isDefined
  override def connectedTo(other: IntervalSet[T]): Boolean =
    other.nonEmptyContinuousSubIntervals.find(otherC => nonEmptyContinuousSubIntervals.find(_.connectedTo(otherC)).isDefined).isDefined

  override def union(other: IntervalSet[T]): IntervalSet[T] = IntervalSet(nonEmptyContinuousSubIntervals ++ other.nonEmptyContinuousSubIntervals)

  override def intersect(other: IntervalSet[T]): IntervalSet[T] = {
    def addIntersection(l: NonEmptyContinuousIntervalSet[T], r: NonEmptyContinuousIntervalSet[T], intersections: List[NonEmptyContinuousIntervalSet[T]]) =
      (l intersect r).nonEmptyContinuousSubIntervals.headOption.map(_ :: intersections).getOrElse(intersections)
      
    def iterate(leftList: List[NonEmptyContinuousIntervalSet[T]], rightList: List[NonEmptyContinuousIntervalSet[T]], intersections: List[NonEmptyContinuousIntervalSet[T]]): List[NonEmptyContinuousIntervalSet[T]] =
      (leftList, rightList) match {
        case (Nil, _) => intersections
        case (_, Nil) => intersections
        case (l :: ls, r :: rs) =>        
          if (l enclosesInterval r) iterate(leftList, rs, r :: intersections)
          else if (r enclosesInterval l) iterate(ls, rightList, l :: intersections)
          else if (IntervalSet.intervalOrder(l, r)) iterate(ls, rightList, addIntersection(l, r, intersections))
          else iterate(leftList, rs, addIntersection(l, r, intersections))
      }    
    
    IntervalSet(iterate(nonEmptyContinuousSubIntervals, other.nonEmptyContinuousSubIntervals, Nil))
  }
  
  override def complement(other: IntervalSet[T]): IntervalSet[T] = {
    val complements = nonEmptyContinuousSubIntervals.flatMap { interval =>
      other.nonEmptyContinuousSubIntervals.foldLeft(List(interval)) { (acc, otherC) =>
        acc.flatMap(a => (a complement otherC).nonEmptyContinuousSubIntervals)
      }
  }
    IntervalSet(complements)
  }
  
  override def toString: String = nonEmptyContinuousSubIntervals.map(_.toString).mkString(" ∪ ")

}
