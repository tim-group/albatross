package com.youdevise.albatross

import Intervals._
import Bounds._

object IntervalSetWorksheet {
	val is = IntervalSet(open(0) to open(10), open(20) to open(30), open(25) to open(34))
                                                  //> is  : com.youdevise.albatross.IntervalSet[Int] = (0...10) ∪ (20...34)
                                                  
	// leftOf rules
  (open(0) to open(10)) leftOf (open(9) to open(15))
                                                  //> res0: Boolean = false
  (open(0) to open(10)) leftOf (open(10) to open(15))
                                                  //> res1: Boolean = true

  (unbounded[Int] to open(0)) leftOf (unbounded[Int] to open(0))
                                                  //> res2: Boolean = false
  (open(0) to open(10)) leftOf (unbounded[Int] to open(0))
                                                  //> res3: Boolean = false
  (unbounded[Int] to open(1)) leftOf (open(0) to open(10))
                                                  //> res4: Boolean = false
                                                  

  // Intersection
	is intersect IntervalSet(open(5) to open(24))
                                                  //> checking (0...10) against (5...24) with intersections List()
                                                  //| checking (20...34) against (5...24) with intersections List((5...10))
                                                  //| res5: com.youdevise.albatross.IntervalSet[Int] = (5...10) ∪ (20...24)
                                                  
  IntervalSet(open(0) to open(1), open(4) to open(8), open(10) to open(11), open(14) to open(16)) intersect IntervalSet(open(2) to open(3), open(3) to open(5), open(6) to open(7), open(9) to open(15))
                                                  //> checking (0...1) against (2...3) with intersections List()
                                                  //| checking (4...8) against (2...3) with intersections List()
                                                  //| checking (4...8) against (3...5) with intersections List()
                                                  //| checking (4...8) against (6...7) with intersections List((4...5))
                                                  //| checking (4...8) against (9...15) with intersections List((6...7), (4...5))
                                                  //| checking (10...11) against (9...15) with intersections List((6...7), (4...5)
                                                  //| )
                                                  //| checking (14...16) against (9...15) with intersections List((10...11), (6...
                                                  //| 7), (4...5))
                                                  //| res6: com.youdevise.albatross.IntervalSet[Int] = (4...5) ∪ (6...7) ∪ (10
                                                  //| ...11) ∪ (14...15)
}