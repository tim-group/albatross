package com.youdevise.albatross

import collection.immutable.Stream.cons

trait Steppable[T] {
  def by(step: T)(implicit discrete: Discrete[T]): Stream[T]
}

trait Discrete[T] {
  val unit: T
  def forward(value: T, step: T): T
  def back(value: T, step: T): T
}

trait Reversible[T] {
  def backwards(): Steppable[T]
}

object Discrete {

  implicit object DiscreteNumeric extends Discrete[Int] {
    val unit = 1
    def forward(value: Int, by: Int): Int = value + by
    def back(value: Int, by: Int): Int = value - by
  }

  implicit def intervalToSteppable[T](interval: Interval[T]) = new Steppable[T] {
    def by(step: T)(implicit discrete: Discrete[T]): Stream[T] = interval match {
      case NonEmptyInterval(lower: Bound[T] with Bounded[T], upper: Bound[T]) => {
        val start = if (lower.isClosed) lower.endpoint else discrete.forward(lower.endpoint, step)
        def streamFrom(i: T): Stream[T] = if (upper.encloses(i)) cons(i, streamFrom(discrete.forward(i, step))) else Stream.empty[T]
        streamFrom(start)
      }
      case EmptyInterval() => Stream.empty[T]
      case _ => throw new UnsupportedOperationException("Cannot stream over interval without a lower bound")
    }
  }

  implicit def intervalToStream[T](interval: Interval[T])(implicit discrete: Discrete[T]): Stream[T] = intervalToSteppable(interval).by(discrete.unit)
  implicit def steppableToStream[T](steppable: Steppable[T])(implicit discrete: Discrete[T]) = steppable.by(discrete.unit)

  implicit def reversible[T, S](interval: Interval[T]) = new Reversible[T] {
    def backwards(): Steppable[T] = new Steppable[T] {
      def by(step: T)(implicit discrete: Discrete[T]): Stream[T] = interval match {
        case NonEmptyInterval(lower: Bound[T], upper: Bound[T] with Bounded[T]) => {
          val start = if (upper.isClosed) upper.endpoint else discrete.back(upper.endpoint, step)
          def streamFrom(i: T): Stream[T] = if (lower.encloses(i)) cons(i, streamFrom(discrete.back(i, step))) else Stream.empty[T]
          streamFrom(start)
        }
        case EmptyInterval() => Stream.empty[T]
        case _ => throw new UnsupportedOperationException("Cannot stream backwards over interval without an upper bound")
      }
    }
  }

}
