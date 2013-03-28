package com.youdevise.albatross

import org.specs2.Specification
import Bounds._

class BoundSpec extends Specification {

  def is =
  openUpperBound ^
  closedUpperBound ^
  openLowerBound ^
  closedLowerBound ^
  leastUpperBound ^
  greatestLowerBound ^
  leastLowerBound ^
  greatestUpperBound ^ end

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
    } ^ end

  def leastUpperBound =
  "The least upper bound" ^
    "Of a closed and an open upper bound with the same endpoint is the open upper bound" ! {
      leastUpper(Some(ClosedUpperBound(0)), Some(OpenUpperBound(0))) must beSome(OpenUpperBound(0))
    } ^
    "Of two closed bounds is the bound with the lowest endpoint" ! {
      leastUpper(Some(ClosedUpperBound(0)), Some(ClosedUpperBound(-1))) must beSome(ClosedUpperBound(-1))
    } ^
    "Of two open bounds is the bound with the lowest endpoint" ! {
      leastUpper(Some(OpenUpperBound(0)), Some(OpenUpperBound(-1))) must beSome(OpenUpperBound(-1))
    } ^
    "Of any bound and no bound is the determinate bound" ! {
      leastUpper(Some(OpenUpperBound(0)), None) must beSome(OpenUpperBound(0))
    } ^ end

  def greatestLowerBound =
    "The greatest lower bound" ^
      "Of a closed and an open lower bound with the same endpoint is the open lower bound" ! {
        greatestLower(Some(ClosedLowerBound(0)), Some(OpenLowerBound(0))) must beSome(OpenLowerBound(0))
      } ^
      "Of two closed bounds is the bound with the highest endpoint" ! {
        greatestLower(Some(ClosedLowerBound(0)), Some(ClosedLowerBound(-1))) must beSome(ClosedLowerBound(0))
      } ^
      "Of two open bounds is the bound with the highest endpoint" ! {
        greatestLower(Some(OpenLowerBound(0)), Some(OpenLowerBound(-1))) must beSome(OpenLowerBound(0))
      } ^
      "Of any bound and no bound is the determinate bound" ! {
        greatestLower(Some(OpenLowerBound(0)), None) must beSome(OpenLowerBound(0))
      } ^ end

  def leastLowerBound =
    "The least lower bound" ^
      "Of a closed and an open lower bound with the same endpoint is the closed lower bound" ! {
        leastLower(Some(ClosedLowerBound(0)), Some(ClosedLowerBound(0))) must beSome(ClosedLowerBound(0))
      } ^
      "Of two closed bounds is the bound with the lowest endpoint" ! {
        leastLower(Some(ClosedLowerBound(0)), Some(ClosedLowerBound(-1))) must beSome(ClosedLowerBound(-1))
      } ^
      "Of two open bounds is the bound with the lowest endpoint" ! {
        leastLower(Some(OpenLowerBound(0)), Some(OpenLowerBound(-1))) must beSome(OpenLowerBound(-1))
      } ^
      "Of any bound and no bound is no bound" ! {
        leastLower(Some(OpenLowerBound(0)), None) must beNone
      } ^ end

  def greatestUpperBound =
    "The greatest upper bound" ^
      "Of a closed and an open upper bound with the same endpoint is the closed upper bound" ! {
        greatestUpper(Some(ClosedUpperBound(0)), Some(OpenUpperBound(0))) must beSome(ClosedUpperBound(0))
      } ^
      "Of two closed bounds is the bound with the highest endpoint" ! {
        greatestUpper(Some(ClosedUpperBound(0)), Some(ClosedUpperBound(-1))) must beSome(ClosedUpperBound(0))
      } ^
      "Of two open bounds is the bound with the highest endpoint" ! {
        greatestUpper(Some(OpenUpperBound(0)), Some(OpenUpperBound(-1))) must beSome(OpenUpperBound(0))
      } ^
      "Of any bound and no bound is no bound" ! {
        greatestUpper(Some(OpenUpperBound(0)), None) must beNone
      } ^ end
}
