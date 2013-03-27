package com.youdevise.albatross

import org.specs2.Specification

class BoundSpec extends Specification {

  def is =
  unboundedLowerBound ^
  unboundedUpperBound ^
  openUpperBound ^
  closedUpperBound ^
  openLowerBound ^
  closedLowerBound ^ end

  def unboundedLowerBound =
  "An unbounded lower bound" ^
    "Is open" ! {
      UnboundedLower[Int].isOpen must beTrue
    } ^
    "Is unbounded" ! {
      UnboundedLower[Int].isUnbounded must beTrue
    } ^
    "Is lower" ! {
      UnboundedLower[Int].isLower must beTrue
    } ^
    "Encloses any value" ! {
      UnboundedLower[Int].encloses(-1000) must beTrue
    } ^
    "Maps to an unbounded lower bound" ! {
      UnboundedLower[Int].map(_ * 2) must_== UnboundedLower[Int]
    } ^ end

  def unboundedUpperBound =
  "An unbounded upper bound" ^
    "Is open" ! {
      UnboundedUpper[Int].isOpen must beTrue
    } ^
    "Is unbounded" ! {
      UnboundedUpper[Int].isUnbounded must beTrue
    } ^
    "Is upper" ! {
      UnboundedUpper[Int].isUpper must beTrue
    } ^
    "Encloses any value" ! {
      UnboundedUpper[Int].encloses(1000) must beTrue
    } ^
    "Maps to an unbounded upper bound" ! {
      UnboundedUpper[Int].map(_ * 2) must_== UnboundedUpper[Int]
    } ^ end

  def openUpperBound =
  "An open upper bound" ^
    "Is open" ! {
      OpenUpperBound(100).isOpen must beTrue
    } ^
    "Is bounded" ! {
      OpenUpperBound(100).isBounded must beTrue
    } ^
    "Is upper" ! {
      OpenUpperBound(100).isUpper must beTrue
    } ^
    "Encloses any value less than its endpoint" ! {
      OpenUpperBound(100).encloses(99) must beTrue
    } ^
    "Does not enclose its endpoint" ! {
      OpenUpperBound(100).encloses(100) must beFalse
    } ^
    "Does not enclose any value greater than its endpoint" ! {
      OpenUpperBound(100).encloses(101) must beFalse
    } ^
    "Maps to an open upper bound" ! {
      OpenUpperBound(100).map(_ * 2) must_== OpenUpperBound(200)
    } ^ end

  def closedUpperBound =
  "A closed upper bound" ^
    "Is closed" ! {
      ClosedUpperBound(100).isClosed must beTrue
    } ^
    "Is bounded" ! {
      ClosedUpperBound(100).isBounded must beTrue
    } ^
    "Is upper" ! {
      ClosedUpperBound(100).isUpper must beTrue
    } ^
    "Encloses any value less than its endpoint" ! {
      ClosedUpperBound(100).encloses(99) must beTrue
    } ^
    "Encloses its endpoint" ! {
      ClosedUpperBound(100).encloses(100) must beTrue
    } ^
    "Does not enclose any value greater than its endpoint" ! {
      ClosedUpperBound(100).encloses(101) must beFalse
    } ^
    "Maps to a closed upper bound" ! {
      ClosedUpperBound(100).map(_ * 2) must_== ClosedUpperBound(200)
    } ^ end

  def openLowerBound =
  "An open lower bound" ^
    "Is open" ! {
      OpenLowerBound(100).isOpen must beTrue
    } ^
    "Is bounded" ! {
      OpenLowerBound(100).isBounded must beTrue
    } ^
    "Is lower" ! {
      OpenLowerBound(100).isLower must beTrue
    } ^
    "Encloses any value greater than its endpoint" ! {
      OpenLowerBound(100).encloses(101) must beTrue
    } ^
    "Does not enclose its endpoint" ! {
      OpenLowerBound(100).encloses(100) must beFalse
    } ^
    "Does not enclose any value less than its endpoint" ! {
      OpenLowerBound(100).encloses(99) must beFalse
    } ^
    "Is innermost to a closed lower bound with the same endpoint" ! {
      OpenLowerBound(100).innerMost(ClosedLowerBound(100)) must_== OpenLowerBound(100)     
    } ^
    "Is innermost to an open lower bound with a lesser endpoint" ! {
      OpenLowerBound(-10).innerMost(OpenLowerBound(-5)) must_== OpenLowerBound(-5)
    } ^
    "Maps to an open lower bound" ! {
      OpenLowerBound(100).map(_ * 2) must_== OpenLowerBound(200)
    } ^ end

  def closedLowerBound =
  "A closed lower bound" ^
    "Is closed" ! {
      ClosedLowerBound(100).isClosed must beTrue
    } ^
    "Is bounded" ! {
      ClosedLowerBound(100).isBounded must beTrue
    } ^
    "Is lower" ! {
      ClosedLowerBound(100).isLower must beTrue
    } ^
    "Encloses any value greater than its endpoint" ! {
      ClosedLowerBound(100).encloses(101) must beTrue
    } ^
    "Encloses its endpoint" ! {
      ClosedLowerBound(100).encloses(100) must beTrue
    } ^
    "Does not enclose any value less than its endpoint" ! {
      ClosedLowerBound(100).encloses(99) must beFalse
    } ^
    "Is outermost to an open lower bound with the same endpoint" ! {
      ClosedLowerBound(100).outerMost(OpenLowerBound(100)) must_== ClosedLowerBound(100)     
    } ^
    "Maps to a closed lower bound" ! {
      ClosedLowerBound(100).map(_ * 2) must_== ClosedLowerBound(200)
    } ^ end
}
