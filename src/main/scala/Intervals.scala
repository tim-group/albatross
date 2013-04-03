package com.youdevise.albatross

import Bounds._

object Intervals {
  def open[T](endpoint: T)(implicit ordering: Ordering[T]) = OpenBoundBuilder[T](endpoint)
  def closed[T](endpoint: T)(implicit ordering: Ordering[T]) = ClosedBoundBuilder[T](endpoint)
  def unbounded[T]()(implicit ordering: Ordering[T]) = UnboundedBuilder[T]
}

trait Interval[T] {
  val isEmpty: Boolean
  val isContinuous: Boolean
  val isASingleton: Boolean  
  val continuousSubIntervals: List[ContinuousInterval[T]]
  
  def encloses(value: T): Boolean
  def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean
  def enclosesBound(bound: Option[Bound[T]]): Boolean
  def enclosesInterval(other: ContinuousInterval[T]): Boolean
  def enclosesInterval(other: Interval[T]): Boolean
  def connectedTo(other: ContinuousInterval[T]): Boolean
  def connectedTo(other: Interval[T]): Boolean

  def intersect(other: ContinuousInterval[T]): Interval[T]    
  def intersect(other: Interval[T]): Interval[T]
  def union(other: ContinuousInterval[T]): Interval[T]    
  def union(other: Interval[T]): Interval[T]

  def complement(other: ContinuousInterval[T]): Interval[T]
  def complement(other: Interval[T]): Interval[T]
}

object Interval {
      
  implicit def intervalOrder[T] = (x: ContinuousInterval[T], y: ContinuousInterval[T]) =>
      if (x.lower == y.lower) (x.upper != y.upper && leastUpper(x.upper, y.upper) == x.upper)
      else (leastLower(x.lower, y.lower) == x.lower)  
      
  def apply[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]): ContinuousInterval[T] = {
    val isEmpty = (for {
      boundedLower <- lower
      boundedUpper <- upper
    } yield !(boundedLower.encloses(boundedUpper.endpoint) && boundedUpper.encloses(boundedLower.endpoint))).getOrElse(false)
    if (isEmpty) throw new IllegalArgumentException("An interval created with the bounds %s will be empty".format(ContinuousInterval.represent(lower, upper)))
    ContinuousInterval(lower, upper)
  }
  
  def apply[T](continuousInterval: ContinuousInterval[T]): ContinuousInterval[T] = continuousInterval
  def apply[T](continuousIntervals: ContinuousInterval[T]*): Interval[T] = apply(continuousIntervals.toSeq)      
  def apply[T](continuousIntervals: Iterable[ContinuousInterval[T]]): Interval[T] = {
    val coalesced = coalesce(continuousIntervals)
    if (coalesced.length == 1) coalesced.head
    else DiscontinuousInterval(coalesced)
  }

  def unapply(interval: ContinuousInterval[_]) = Some((interval.lower, interval.upper))

  private def coalesce[T](intervals: Iterable[ContinuousInterval[T]]): List[ContinuousInterval[T]] = {
    val ordered = intervals.toList.distinct.sortWith(intervalOrder[T])
    if (ordered.size < 2) ordered
    else {
      def coalesce(coalesced: List[ContinuousInterval[T]], uncoalesced: List[ContinuousInterval[T]], left: ContinuousInterval[T]): List[ContinuousInterval[T]] = uncoalesced match {
        case Nil => left :: coalesced
        case right :: tail => if (left connectedTo right) 
          coalesce(coalesced, tail, ContinuousInterval(leastLower(left.lower, right.lower), greatestUpper(left.upper, right.upper)))
          else coalesce(left :: coalesced, tail, right)
      }
      val reverseOrdered = coalesce(Nil, ordered.tail, ordered.head)
      reverseOrdered.reverse
    }
  }
}

sealed case class ContinuousInterval[T] private[albatross](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]) extends Interval[T] {
  
  override val isEmpty = false
  override val isContinuous = true
  
  override val isASingleton: Boolean = (upper, lower) match {
    case (Some(a), Some(b)) => a.isClosed && b.isClosed && a.endpoint == b.endpoint
    case _ => false
  }
  
  lazy val continuousSubIntervals: List[ContinuousInterval[T]] = List(this)

  override def encloses(value: T): Boolean = lower.map(_.encloses(value)).getOrElse(true) && upper.map(_.encloses(value)).getOrElse(true)

  override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean =
    encloses(values.min) && encloses(values.max)

  override def enclosesBound(bound: Option[Bound[T]]): Boolean = bound.map(b => encloses(b.endpoint)).getOrElse(false)

  override def enclosesInterval(other: ContinuousInterval[T]): Boolean =
    (leastLower(lower, other.lower) == ContinuousInterval.this.lower) && (greatestUpper(upper, other.upper) == ContinuousInterval.this.upper)
    
  override def enclosesInterval(other: Interval[T]): Boolean =
    other.continuousSubIntervals.forall(enclosesInterval(_))

  override def connectedTo(other: ContinuousInterval[T]): Boolean =
    (ContinuousInterval.this enclosesBound other.lower) || (ContinuousInterval.this enclosesBound other.upper) ||
         (other enclosesBound lower) || (other enclosesBound upper)
         
  override def connectedTo(other: Interval[T]): Boolean =
    other.continuousSubIntervals.find(connectedTo(_)).isDefined         

  override def intersect(other: ContinuousInterval[T]): Interval[T] =
    if (!connectedTo(other)) Interval(Iterable.empty)
    else ContinuousInterval(greatestLower(lower, other.lower), leastUpper(upper, other.upper))
    
  override def intersect(other: Interval[T]): Interval[T] =
    Interval(other.continuousSubIntervals.map(_ intersect this).flatMap(_.continuousSubIntervals))

  override def union(other: ContinuousInterval[T]): Interval[T] =
    if (connectedTo(other)) ContinuousInterval(leastLower(lower, other.lower), greatestUpper(upper, other.upper))
    else Interval(Seq(this, other))
    
  override def union(other: Interval[T]): Interval[T] = 
    if (other.isContinuous) this union other.continuousSubIntervals.head
    else other union this

  override def complement(other: ContinuousInterval[T]): Interval[T] =
    if (ContinuousInterval.this == other) Interval()
    else if (other enclosesInterval ContinuousInterval.this) Interval()
    else if (!connectedTo(other)) this 
    else if (ContinuousInterval.this enclosesInterval other) Interval(
      ContinuousInterval(lower, other.lower.map(_.inverse)),
      ContinuousInterval(other.upper.map(_.inverse), upper))
    else if (ContinuousInterval.this enclosesBound other.lower) ContinuousInterval(lower, other.lower.map(_.inverse))
    else ContinuousInterval(other.upper.map(_.inverse), upper)
    
  override def complement(other: Interval[T]): Interval[T] =
    other.continuousSubIntervals.foldLeft(Interval(this).asInstanceOf[Interval[T]])(_ complement _)

  override def toString: String = ContinuousInterval.represent(lower, upper)
}

object ContinuousInterval {
  def represent[T](lower: MaybeLowerBound[T], upper: MaybeUpperBound[T]): String = "%s...%s".format(lower.map(_.toString).getOrElse("∞"), upper.map(_.toString).getOrElse("∞"))
}

private[albatross] sealed case class DiscontinuousInterval[T](continuousSubIntervals: List[ContinuousInterval[T]]) extends Interval[T] {
  override val isASingleton: Boolean = false
  override val isEmpty: Boolean = continuousSubIntervals.isEmpty
  override val isContinuous: Boolean = false
  
  override def encloses(value: T): Boolean = continuousSubIntervals.find(_.encloses(value)).isDefined
  override def enclosesAll(values: Iterable[T])(implicit ord: Ordering[T]): Boolean = values.forall(encloses(_))
  
  override def enclosesInterval(other: ContinuousInterval[T]): Boolean = continuousSubIntervals.find(_.enclosesInterval(other)).isDefined
  
  override def enclosesInterval(other: Interval[T]): Boolean =
    (DiscontinuousInterval.this intersect other) == other
    
  override def enclosesBound(other: Option[Bound[T]]): Boolean = continuousSubIntervals.find(_.enclosesBound(other)).isDefined
  override def connectedTo(other: ContinuousInterval[T]): Boolean = continuousSubIntervals.find(_.connectedTo(other)).isDefined
  override def connectedTo(other: Interval[T]): Boolean =
    other.continuousSubIntervals.find(otherC => continuousSubIntervals.find(_.connectedTo(otherC)).isDefined).isDefined

  override def union(other: ContinuousInterval[T]): Interval[T] = Interval(other :: continuousSubIntervals)
  override def union(other: Interval[T]): Interval[T] = Interval(continuousSubIntervals ++ other.continuousSubIntervals)
  
  override def intersect(other: ContinuousInterval[T]): Interval[T] = Interval(continuousSubIntervals.flatMap(self => (self intersect other).continuousSubIntervals))
  override def intersect(other: Interval[T]): Interval[T] = {
    def addIntersection(l: ContinuousInterval[T], r: ContinuousInterval[T], intersections: List[ContinuousInterval[T]]) =
      (l intersect r).continuousSubIntervals.headOption.map(_ :: intersections).getOrElse(intersections)
      
    def iterate(leftList: List[ContinuousInterval[T]], rightList: List[ContinuousInterval[T]], intersections: List[ContinuousInterval[T]]): List[ContinuousInterval[T]] =
      (leftList, rightList) match {
        case (Nil, _) => intersections
        case (_, Nil) => intersections
        case (l :: ls, r :: rs) =>        
          if (l enclosesInterval r) iterate(leftList, rs, r :: intersections)
          else if (r enclosesInterval l) iterate(ls, rightList, l :: intersections)
          else if (Interval.intervalOrder(l, r)) iterate(ls, rightList, addIntersection(l, r, intersections))
          else iterate(leftList, rs, addIntersection(l, r, intersections))
      }    
    
    Interval(iterate(continuousSubIntervals, other.continuousSubIntervals, Nil))
  }

  override def complement(other: ContinuousInterval[T]): Interval[T] = {
    val complements = continuousSubIntervals.flatMap { interval =>
      (other complement interval).continuousSubIntervals
    }
    Interval(complements)
  }
  
  override def complement(other: Interval[T]): Interval[T] = {
    val complements = continuousSubIntervals.flatMap { interval =>
      other.continuousSubIntervals.foldLeft(List(interval)) { (acc, otherC) =>
        acc.flatMap(a => (a complement otherC).continuousSubIntervals)
      }
  }
    Interval(complements)
  }
  
  override def toString: String = continuousSubIntervals.map(_.toString).mkString(" ∪ ")

}
