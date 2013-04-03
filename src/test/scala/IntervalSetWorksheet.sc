package com.youdevise.albatross

import Intervals._
import Bounds._

object IntervalSetWorksheet {
	val is = IntervalSet(open(0) to open(10), open(20) to open(30), open(25) to open(34))
                                                  //> is  : com.youdevise.albatross.IntervalSet[Int] = (0...10) ∪ (20...34)

  // Intersections
	is intersect IntervalSet(open(5) to open(24))
                                                  //> res0: com.youdevise.albatross.IntervalSet[Int] = (5...10) ∪ (20...24)
                                                  
  IntervalSet(open(0) to open(1), open(4) to open(8), open(10) to open(11), open(14) to open(16)) intersect IntervalSet(open(2) to open(3), open(3) to open(5), open(6) to open(7), open(9) to open(15))
                                                  //> res1: com.youdevise.albatross.IntervalSet[Int] = (4...5) ∪ (6...7) ∪ (10
                                                  //| ...11) ∪ (14...15)
}