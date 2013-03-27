package com.youdevise.albatross

import org.specs2.Specification

class BoundSpec extends Specification {

  def is =
  openUpperBound ^
  closedUpperBound ^
  openLowerBound ^
  closedLowerBound ^ end

  def openUpperBound =
  "An open upper bound" ^
    "Is open" ! {
      OpenUpperBound(100).isOpen must beTrue
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
    } ^ end

  def closedUpperBound =
  "A closed upper bound" ^
    "Is closed" ! {
      ClosedUpperBound(100).isClosed must beTrue
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
    } ^ end

  def openLowerBound =
  "An open lower bound" ^
    "Is open" ! {
      OpenLowerBound(100).isOpen must beTrue
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
    } ^ end

  def closedLowerBound =
  "A closed lower bound" ^
    "Is closed" ! {
      ClosedLowerBound(100).isClosed must beTrue
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
    } ^ end
}
