package com.youdevise.albatross

import Intervals._
import Bounds._

object IntervalSetWorksheet {
	val is = (open(0) to open(10)) union (open(20) to open(30)) union (open(25) to open(34))
                                                  //> is  : com.youdevise.albatross.IntervalSet[Int] = (0...10) ∪ (20...34)

  // Intersection
	is intersect (open(5) to open(24))        //> res0: com.youdevise.albatross.IntervalSet[Int] = (5...10) ∪ (20...24)
                                                  
  IntervalSet(open(0) to open(1), open(4) to open(8), open(10) to open(11), open(14) to open(16)) intersect IntervalSet(open(2) to open(3), open(3) to open(5), open(6) to open(7), open(9) to open(15))
                                                  //> res1: com.youdevise.albatross.IntervalSet[Int] = (4...5) ∪ (6...7) ∪ (10
                                                  //| ...11) ∪ (14...15)
                                                  
  // Unions
  is union IntervalSet(open(3) to open(21))       //> res2: com.youdevise.albatross.IntervalSet[Int] = (0...34)
  
  // Intersection and union
  (open(0) to open(10)) intersect (open(2) to open(3)) union (open(7) to open(8))
                                                  //> res3: com.youdevise.albatross.IntervalSet[Int] = (2...3) ∪ (7...8)
  ((open(0) to open(10)) intersect (open(2) to open(3))) union (open(7) to open(8))
                                                  //> res4: com.youdevise.albatross.IntervalSet[Int] = (2...3) ∪ (7...8)
  (open(0) to open(10)) intersect ((open(2) to open(3)) union (open(7) to open(8)))
                                                  //> res5: com.youdevise.albatross.IntervalSet[Int] = (2...3) ∪ (7...8)
}