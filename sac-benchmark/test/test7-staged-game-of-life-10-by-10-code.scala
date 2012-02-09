package scala.virtualization.lms
package epfl
package test7
package original

import test7.original.Conversions._
import test7.original.Operations._
import test7.original.SpecificOperations._

/*****************************************
  Emitting Generated Code                  
*******************************************/
class Experiment extends ((scala.virtualization.lms.epfl.test7.original.MDArray[Int])=>(scala.virtualization.lms.epfl.test7.original.MDArray[Int])) {
  def apply(x1:scala.virtualization.lms.epfl.test7.original.MDArray[Int]): scala.virtualization.lms.epfl.test7.original.MDArray[Int] = {
    // Shape: V2=[10  10] and S2=[2]
    val x2: MDArray[Int] = internalReshape(2::Nil, Array(10, 10), "knownAtCompileTime")
    // RuntimeCheck : POST:   V2 = [10  10]                                          from Bubble up value for Sym(2) <- Reshape(Sym(2), Sym(1))
    // RuntimeCheck : POST:   S2 = [2]                                               from Bubble up shape for Sym(2) <- Reshape(Sym(2), Sym(1))
    // RuntimeCheck : PRE:    length(S2) = length([u2946])                           from Reshape(Sym(2), Sym(1))
    // RuntimeCheck : PRE:    prod(V2) = prod(S1)                                    from Reshape(Sym(2), Sym(1))
    // Shape: V3=[u679  u678  u677  ...  u582  u581  u580] and S3=[10  10]
    val x3: MDArray[Int] = reshape(x2, x1)
    // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- Shape(Sym(3))
    // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- Shape(Sym(3))
    // Shape: V6=[10  10] and S6=[2]
    val x6: MDArray[Int] = shape(x3)
    // RuntimeCheck : POST:   V6 = [10  10]                                          from Bubble up value for Sym(6) <- GenArrayWith(Sym(6) - Sym(68))
    // RuntimeCheck : POST:   S6 = [2]                                               from Bubble up shape for Sym(6) <- GenArrayWith(Sym(6) - Sym(68))
    // RuntimeCheck : POST:   V68 = [u138]                                           from Bubble up value for Sym(68) <- GenArrayWith(Sym(6) - Sym(68))
    // RuntimeCheck : POST:   S68 = []                                               from Bubble up shape for Sym(68) <- GenArrayWith(Sym(6) - Sym(68))
    // RuntimeCheck : PRE:    S6 = [u2673]                                           from GenArrayWith(Sym(6) - Sym(68))
    // RuntimeCheck : PRE:    S6 = S11                                               from GenArrayWith(Sym(6) - Sym(68))
    // RuntimeCheck : PRE:    V6(:length(V11)) < V11                                 from GenArrayWith(Sym(6) - Sym(68))
    // RuntimeCheck : PRE:    V6(length(V11):) = S68                                 from GenArrayWith(Sym(6) - Sym(68))
    // Shape: V69=[u579  u578  u577  ...  u482  u481  u480] and S69=[10  10]
    
    val x69: MDArray[Int] = {
      val opName: String = "genarray"
      var result: Array[Int] = null
      var rshape: Array[Int] = null
      // Shape: V7=[0] and S7=[1]
      val x7: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V6 = [10  10]                                          from Bubble up value for Sym(6) <- Shape(Sym(6))
      // RuntimeCheck : POST:   S6 = [2]                                               from Bubble up shape for Sym(6) <- Shape(Sym(6))
      // Shape: V8=[2] and S8=[1]
      val x8: MDArray[Int] = shape(x6)
      // RuntimeCheck : POST:   V8 = [2]                                               from Bubble up value for Sym(8) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : POST:   S8 = [1]                                               from Bubble up shape for Sym(8) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : PRE:    length(S7) = length([u2687])                           from Sel(Sym(7), Sym(8))
      // RuntimeCheck : PRE:    S8(:length(V7)) < V7                                   from Sel(Sym(7), Sym(8))
      // Shape: V9=[2] and S9=[]
      
      // Shape: V9=[2] and S9=[]
      val x9: Int = x8.content()(flatten(shape(x8), x7, "sel"))
      // Shape: V10=[0] and S10=[]
      val x10: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : POST:   V9 = [2]                                               from Bubble up value for Sym(9) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : POST:   S9 = []                                                from Bubble up shape for Sym(9) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : PRE:    S9 = []                                                from Values(Sym(10), Sym(9))
      // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(9))
      // Shape: V11=[0  0] and S11=[2]
      val x11: MDArray[Int] = {
        val result = new Array[Int](x9)
        for(i <- List.range(0, result.length))
        result(i) = x10
        internalReshape(x9::Nil, result, "values")
      }
      // Shape: V12=[1] and S12=[]
      val x12: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : POST:   V9 = [2]                                               from Bubble up value for Sym(9) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : POST:   S9 = []                                                from Bubble up shape for Sym(9) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : PRE:    S9 = []                                                from Values(Sym(12), Sym(9))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(9))
      // Shape: V14=[1  1] and S14=[2]
      val x14: MDArray[Int] = {
        val result = new Array[Int](x9)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x9::Nil, result, "values")
      }
      // Shape: V4=[u5] and S4=[]
      val x4: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V4 = [u5]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
      // Shape: V5=[u118] and S5=[]
      val x5: Boolean = x4
      // RuntimeCheck : POST:   V5 = [u118]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
      // Shape: V15=[u117] and S15=[]
      val x15: Boolean = x5
      // RuntimeCheck : POST:   V15 = [u117]                                           from Bubble up value for Sym(15) <- ToValue(Sym(15))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- ToValue(Sym(15))
      // RuntimeCheck : PRE:    length(S15) = length([])                               from ToValue(Sym(15))
      // Shape: V16=[u116] and S16=[]
      val x16: Boolean = x15
      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : POST:   V6 = [10  10]                                          from Bubble up value for Sym(6) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : POST:   S6 = [2]                                               from Bubble up shape for Sym(6) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : PRE:    S6 = S12 OR S12 = []                                   from InfixOp(-: Sym(6) and Sym(12))
      // Shape: V13=[u179  u178] and S13=[2]
      val x13: MDArray[Int] = {
        val result = new Array[Int](shape(x6).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x6.content()(i) -  x12
        internalReshape(shape(x6), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V67 = [u139]                                           from Bubble up value for Sym(67) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S67 = []                                               from Bubble up shape for Sym(67) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   V17 = [u794(<10)  u795(<10)]                           from Bubble up value for Sym(17) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S17 = [2]                                              from Bubble up shape for Sym(17) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   V11 = [0  0]                                           from Bubble up value for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S11 = [2]                                              from Bubble up shape for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   V14 = [1  1]                                           from Bubble up value for Sym(14) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S14 = [2]                                              from Bubble up shape for Sym(14) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   V16 = [u116]                                           from Bubble up value for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S16 = []                                               from Bubble up shape for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   V13 = [u179  u178]                                     from Bubble up value for Sym(13) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S13 = [2]                                              from Bubble up shape for Sym(13) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   V16 = [u116]                                           from Bubble up value for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S16 = []                                               from Bubble up shape for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   V11 = [0  0]                                           from Bubble up value for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : POST:   S11 = [2]                                              from Bubble up shape for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : PRE:    length(S11) = length([u2674])                          from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : PRE:    S16 = []                                               from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : PRE:    S16 = []                                               from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : PRE:    S13 = S11                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : PRE:    S14 = S11                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : PRE:    S11 = S11                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // RuntimeCheck : PRE:    V11 < V13                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      // Shape: V68=[u138] and S68=[]
      // with: With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(13) step=Sym(14) width=Sym(11)  Sym(17) => Sym(67))
      val lb0: Int = x11.content()(0)
      val ub0: Int = x13.content()(0)
      val step0: Int = x14.content()(0)
      val width0: Int = x11.content()(0)
      val ll0: Int = if (x16) lb0 + 1 else lb0
      val ul0: Int = if (x16) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x11.content()(1)
          val ub1: Int = x13.content()(1)
          val step1: Int = x14.content()(1)
          val width1: Int = x11.content()(1)
          val ll1: Int = if (x16) lb1 + 1 else lb1
          val ul1: Int = if (x16) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val x17: MDArray[Int] = iv0::iv1::Nil
              val iv: MDArray[Int] = x17
              val feval: MDArray[Int] = {
                // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- Sel(Sym(17), Sym(3))
                // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- Sel(Sym(17), Sym(3))
                // RuntimeCheck : POST:   V17 = [u794(<10)  u795(<10)]                           from Bubble up value for Sym(17) <- Sel(Sym(17), Sym(3))
                // RuntimeCheck : POST:   S17 = [2]                                              from Bubble up shape for Sym(17) <- Sel(Sym(17), Sym(3))
                // RuntimeCheck : PRE:    length(S17) = length([u2939])                          from Sel(Sym(17), Sym(3))
                // RuntimeCheck : PRE:    S3(:length(V17)) < V17                                 from Sel(Sym(17), Sym(3))
                // Shape: V54=[u791] and S54=[]
                
                // Shape: V54=[u791] and S54=[]
                val x54: Int = x3.content()(flatten(shape(x3), x17, "sel"))
                // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(===: Sym(54) and Sym(12))
                // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(===: Sym(54) and Sym(12))
                // RuntimeCheck : POST:   V54 = [u791]                                           from Bubble up value for Sym(54) <- InfixOp(===: Sym(54) and Sym(12))
                // RuntimeCheck : POST:   S54 = []                                               from Bubble up shape for Sym(54) <- InfixOp(===: Sym(54) and Sym(12))
                // RuntimeCheck : PRE:    S54 = S12 OR S12 = []                                  from InfixOp(===: Sym(54) and Sym(12))
                // Shape: V55=[u790] and S55=[]
                val x55: Boolean = {
                  val result = new Array[Boolean](shape(x54).content().foldLeft(1)((a,b) => a*b))
                  for(i <- List.range(0, result.length))
                  result(i) = x54.content()(i) ===  x12
                  internalReshape(shape(x54), result, "infixOpAA")
                }
                // RuntimeCheck : POST:   V55 = [u790]                                           from Bubble up value for Sym(55) <- ToValue(Sym(55))
                // RuntimeCheck : POST:   S55 = []                                               from Bubble up shape for Sym(55) <- ToValue(Sym(55))
                // RuntimeCheck : PRE:    length(S55) = length([])                               from ToValue(Sym(55))
                // Shape: V56=[u169] and S56=[]
                val x56: Boolean = x55
                val x67 = if (x56) {
                  // RuntimeCheck : POST:   V47 = [u157]                                           from Bubble up value for Sym(47) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : POST:   S47 = []                                               from Bubble up shape for Sym(47) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : POST:   S46 = []                                               from Bubble up shape for Sym(46) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : POST:   S45 = []                                               from Bubble up shape for Sym(45) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : POST:   V50 = [u776]                                           from Bubble up value for Sym(50) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : POST:   S50 = []                                               from Bubble up shape for Sym(50) <- FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : PRE:    S10 = S50                                              from FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // RuntimeCheck : PRE:    S47 = S50                                              from FoldArrayWith(Sym(10), fold (Sym(45), Sym(46)) => Sym(47), Sym(50))
                  // Shape: V51=[u156] and S51=[]
                  
                  val x51: Int = {
                    val opName: String = "fold"
                    var result: MDArray[Int] = x10
                    val foldFunction: (MDArray[Int], MDArray[Int]) => MDArray[Int] = (x45, x46) => {
                      // RuntimeCheck : POST:   S46 = []                                               from Bubble up shape for Sym(46) <- ScalarOperator Sym(45) + Sym(46)
                      // RuntimeCheck : POST:   S45 = []                                               from Bubble up shape for Sym(45) <- ScalarOperator Sym(45) + Sym(46)
                      // RuntimeCheck : PRE:    S45 = []                                               from ScalarOperator Sym(45) + Sym(46)
                      // RuntimeCheck : PRE:    S46 = []                                               from ScalarOperator Sym(45) + Sym(46)
                      // Shape: V47=[u157] and S47=[]
                      val x47: Int = ((a: Int, b: Int) => a + b)(x45, x46)
                      x47
                    }
                    // Shape: V23=[u7] and S23=[]
                    val x23: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
                    // RuntimeCheck : POST:   V23 = [u7]                                             from Bubble up value for Sym(23) <- ToValue(Sym(23))
                    // RuntimeCheck : POST:   S23 = []                                               from Bubble up shape for Sym(23) <- ToValue(Sym(23))
                    // RuntimeCheck : PRE:    length(S23) = length([])                               from ToValue(Sym(23))
                    // Shape: V24=[u89] and S24=[]
                    val x24: Boolean = x23
                    // RuntimeCheck : POST:   V24 = [u89]                                            from Bubble up value for Sym(24) <- FromValue(Sym(24))
                    // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- FromValue(Sym(24))
                    // Shape: V30=[u88] and S30=[]
                    val x30: Boolean = x24
                    // RuntimeCheck : POST:   V30 = [u88]                                            from Bubble up value for Sym(30) <- ToValue(Sym(30))
                    // RuntimeCheck : POST:   S30 = []                                               from Bubble up shape for Sym(30) <- ToValue(Sym(30))
                    // RuntimeCheck : PRE:    length(S30) = length([])                               from ToValue(Sym(30))
                    // Shape: V31=[u87] and S31=[]
                    val x31: Boolean = x30
                    // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- Dim(Sym(3))
                    // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- Dim(Sym(3))
                    // Shape: V18=[2] and S18=[]
                    val x18: Int = dim(x3)
                    // RuntimeCheck : POST:   V18 = [2]                                              from Bubble up value for Sym(18) <- FromValue(Sym(18))
                    // RuntimeCheck : POST:   S18 = []                                               from Bubble up shape for Sym(18) <- FromValue(Sym(18))
                    // Shape: V19=[2] and S19=[]
                    val x19: Int = x18
                    // Shape: V20=[3] and S20=[]
                    val x20: Int = internalReshape(Nil, Array(3), "knownAtCompileTime")
                    // RuntimeCheck : POST:   V20 = [3]                                              from Bubble up value for Sym(20) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   V19 = [2]                                              from Bubble up value for Sym(19) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   S19 = []                                               from Bubble up shape for Sym(19) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : PRE:    S19 = []                                               from Values(Sym(20), Sym(19))
                    // RuntimeCheck : PRE:    S20 = []                                               from Values(Sym(20), Sym(19))
                    // Shape: V21=[3  3] and S21=[2]
                    val x21: MDArray[Int] = {
                      val result = new Array[Int](x19)
                      for(i <- List.range(0, result.length))
                      result(i) = x20
                      internalReshape(x19::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- GenArrayWith(Sym(21) - Sym(35))
                    // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- GenArrayWith(Sym(21) - Sym(35))
                    // RuntimeCheck : POST:   V35 = [u748]                                           from Bubble up value for Sym(35) <- GenArrayWith(Sym(21) - Sym(35))
                    // RuntimeCheck : POST:   S35 = []                                               from Bubble up shape for Sym(35) <- GenArrayWith(Sym(21) - Sym(35))
                    // RuntimeCheck : PRE:    S21 = [u2917]                                          from GenArrayWith(Sym(21) - Sym(35))
                    // RuntimeCheck : PRE:    S21 = S27                                              from GenArrayWith(Sym(21) - Sym(35))
                    // RuntimeCheck : PRE:    V21(:length(V27)) < V27                                from GenArrayWith(Sym(21) - Sym(35))
                    // RuntimeCheck : PRE:    V21(length(V27):) = S35                                from GenArrayWith(Sym(21) - Sym(35))
                    // Shape: V36=[u747  u746  u745  ...  u741  u740  u739] and S36=[3  3]
                    
                    val x36: MDArray[Int] = {
                      val opName: String = "genarray"
                      var result: Array[Int] = null
                      var rshape: Array[Int] = null
                      // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- Shape(Sym(21))
                      // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- Shape(Sym(21))
                      // Shape: V25=[2] and S25=[1]
                      val x25: MDArray[Int] = shape(x21)
                      // RuntimeCheck : POST:   V25 = [2]                                              from Bubble up value for Sym(25) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   S25 = [1]                                              from Bubble up shape for Sym(25) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : PRE:    length(S7) = length([u2931])                           from Sel(Sym(7), Sym(25))
                      // RuntimeCheck : PRE:    S25(:length(V7)) < V7                                  from Sel(Sym(7), Sym(25))
                      // Shape: V26=[2] and S26=[]
                      
                      // Shape: V26=[2] and S26=[]
                      val x26: Int = x25.content()(flatten(shape(x25), x7, "sel"))
                      // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   V26 = [2]                                              from Bubble up value for Sym(26) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   S26 = []                                               from Bubble up shape for Sym(26) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : PRE:    S26 = []                                               from Values(Sym(10), Sym(26))
                      // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(26))
                      // Shape: V27=[0  0] and S27=[2]
                      val x27: MDArray[Int] = {
                        val result = new Array[Int](x26)
                        for(i <- List.range(0, result.length))
                        result(i) = x10
                        internalReshape(x26::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : PRE:    S21 = S12 OR S12 = []                                  from InfixOp(-: Sym(21) and Sym(12))
                      // Shape: V28=[u751  u750] and S28=[2]
                      val x28: MDArray[Int] = {
                        val result = new Array[Int](shape(x21).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x21.content()(i) -  x12
                        internalReshape(shape(x21), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   V26 = [2]                                              from Bubble up value for Sym(26) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   S26 = []                                               from Bubble up shape for Sym(26) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : PRE:    S26 = []                                               from Values(Sym(12), Sym(26))
                      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(26))
                      // Shape: V29=[1  1] and S29=[2]
                      val x29: MDArray[Int] = {
                        val result = new Array[Int](x26)
                        for(i <- List.range(0, result.length))
                        result(i) = x12
                        internalReshape(x26::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(17) and Sym(12))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(17) and Sym(12))
                      // RuntimeCheck : POST:   V17 = [u794(<10)  u795(<10)]                           from Bubble up value for Sym(17) <- InfixOp(-: Sym(17) and Sym(12))
                      // RuntimeCheck : POST:   S17 = [2]                                              from Bubble up shape for Sym(17) <- InfixOp(-: Sym(17) and Sym(12))
                      // RuntimeCheck : PRE:    S17 = S12 OR S12 = []                                  from InfixOp(-: Sym(17) and Sym(12))
                      // Shape: V22=[u689  u688] and S22=[2]
                      val x22: MDArray[Int] = {
                        val result = new Array[Int](shape(x17).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x17.content()(i) -  x12
                        internalReshape(shape(x17), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V34 = [u749]                                           from Bubble up value for Sym(34) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S34 = []                                               from Bubble up shape for Sym(34) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S32 = [2]                                              from Bubble up shape for Sym(32) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   V27 = [0  0]                                           from Bubble up value for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S27 = [2]                                              from Bubble up shape for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   V29 = [1  1]                                           from Bubble up value for Sym(29) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S29 = [2]                                              from Bubble up shape for Sym(29) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   V28 = [u751  u750]                                     from Bubble up value for Sym(28) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S28 = [2]                                              from Bubble up shape for Sym(28) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   V27 = [0  0]                                           from Bubble up value for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : POST:   S27 = [2]                                              from Bubble up shape for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : PRE:    length(S27) = length([u2918])                          from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : PRE:    S28 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : PRE:    S29 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : PRE:    S27 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // RuntimeCheck : PRE:    V27 < V28                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      // Shape: V35=[u748] and S35=[]
                      // with: With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(28) step=Sym(29) width=Sym(27)  Sym(32) => Sym(34))
                      val lb0: Int = x27.content()(0)
                      val ub0: Int = x28.content()(0)
                      val step0: Int = x29.content()(0)
                      val width0: Int = x27.content()(0)
                      val ll0: Int = if (x31) lb0 + 1 else lb0
                      val ul0: Int = if (x31) ub0 else ub0 + 1
                      for (iv0 <- List.range(ll0, ul0)) {
                        if ((iv0 - lb0) % step0 <= width0) {
                          val lb1: Int = x27.content()(1)
                          val ub1: Int = x28.content()(1)
                          val step1: Int = x29.content()(1)
                          val width1: Int = x27.content()(1)
                          val ll1: Int = if (x31) lb1 + 1 else lb1
                          val ul1: Int = if (x31) ub1 else ub1 + 1
                          for (iv1 <- List.range(ll1, ul1)) {
                            if ((iv1 - lb1) % step1 <= width1) {
                              val x32: MDArray[Int] = iv0::iv1::Nil
                              val iv: MDArray[Int] = x32
                              val feval: MDArray[Int] = {
                                // RuntimeCheck : POST:   V22 = [u689  u688]                                     from Bubble up value for Sym(22) <- InfixOp(+: Sym(32) and Sym(22))
                                // RuntimeCheck : POST:   S22 = [2]                                              from Bubble up shape for Sym(22) <- InfixOp(+: Sym(32) and Sym(22))
                                // RuntimeCheck : POST:   S32 = [2]                                              from Bubble up shape for Sym(32) <- InfixOp(+: Sym(32) and Sym(22))
                                // RuntimeCheck : PRE:    S32 = S22 OR S22 = []                                  from InfixOp(+: Sym(32) and Sym(22))
                                // Shape: V33=[u788(<10)  u789(<10)] and S33=[2]
                                val x33: MDArray[Int] = {
                                  val result = new Array[Int](shape(x32).content().foldLeft(1)((a,b) => a*b))
                                  for(i <- List.range(0, result.length))
                                  result(i) = x32.content()(i) +  x22.content()(i)
                                  internalReshape(shape(x32), result, "infixOpAA")
                                }
                                // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- Sel(Sym(33), Sym(3))
                                // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- Sel(Sym(33), Sym(3))
                                // RuntimeCheck : POST:   V33 = [u788(<10)  u789(<10)]                           from Bubble up value for Sym(33) <- Sel(Sym(33), Sym(3))
                                // RuntimeCheck : POST:   S33 = [2]                                              from Bubble up shape for Sym(33) <- Sel(Sym(33), Sym(3))
                                // RuntimeCheck : PRE:    length(S33) = length([u2935])                          from Sel(Sym(33), Sym(3))
                                // RuntimeCheck : PRE:    S3(:length(V33)) < V33                                 from Sel(Sym(33), Sym(3))
                                // Shape: V34=[u749] and S34=[]
                                
                                // Shape: V34=[u749] and S34=[]
                                val x34: Int = x3.content()(flatten(shape(x3), x33, "sel"))
                                x34
                              }
                              // the action of this loop:
                              if (result == null) {
                                // create the array and shape
                                result = new Array[Int](x21.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                rshape = shape(feval).content()
                              } else {
                                // check shape -- this WILL be redundant due to runtime checks
                                if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                              }
                              // copy new content
                              val mainIndex: Int = flatten(x21 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                              for (innerIndex <- List.range(0, rshape.length)) {
                                result(mainIndex + innerIndex) = feval(innerIndex)
                              }
                            } // if ((iv0 ...
                          } // for (iv0 ...
                        } // if ((iv1 ...
                      } // for (iv1 ...
                      internalReshape(x21 ::: rshape.toList, result, opName)
                    }
                    
                    // RuntimeCheck : POST:   V36 = [u747  u746  u745  u744  u743  u742  u741  u740  u739]     from Bubble up value for Sym(36) <- Dim(Sym(36))
                    // RuntimeCheck : POST:   S36 = [3  3]                                           from Bubble up shape for Sym(36) <- Dim(Sym(36))
                    // Shape: V37=[2] and S37=[]
                    val x37: Int = dim(x36)
                    // RuntimeCheck : POST:   V37 = [2]                                              from Bubble up value for Sym(37) <- FromValue(Sym(37))
                    // RuntimeCheck : POST:   S37 = []                                               from Bubble up shape for Sym(37) <- FromValue(Sym(37))
                    // Shape: V38=[2] and S38=[]
                    val x38: Int = x37
                    // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(38))
                    // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(38))
                    // RuntimeCheck : POST:   V38 = [2]                                              from Bubble up value for Sym(38) <- Values(Sym(10), Sym(38))
                    // RuntimeCheck : POST:   S38 = []                                               from Bubble up shape for Sym(38) <- Values(Sym(10), Sym(38))
                    // RuntimeCheck : PRE:    S38 = []                                               from Values(Sym(10), Sym(38))
                    // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(38))
                    // Shape: V39=[0  0] and S39=[2]
                    val x39: MDArray[Int] = {
                      val result = new Array[Int](x38)
                      for(i <- List.range(0, result.length))
                      result(i) = x10
                      internalReshape(x38::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V36 = [u747  u746  u745  u744  u743  u742  u741  u740  u739]     from Bubble up value for Sym(36) <- Shape(Sym(36))
                    // RuntimeCheck : POST:   S36 = [3  3]                                           from Bubble up shape for Sym(36) <- Shape(Sym(36))
                    // Shape: V40=[3  3] and S40=[2]
                    val x40: MDArray[Int] = shape(x36)
                    // RuntimeCheck : POST:   V39 = [0  0]                                           from Bubble up value for Sym(39) <- Shape(Sym(39))
                    // RuntimeCheck : POST:   S39 = [2]                                              from Bubble up shape for Sym(39) <- Shape(Sym(39))
                    // Shape: V41=[2] and S41=[1]
                    val x41: MDArray[Int] = shape(x39)
                    // RuntimeCheck : POST:   V41 = [2]                                              from Bubble up value for Sym(41) <- Sel(Sym(7), Sym(41))
                    // RuntimeCheck : POST:   S41 = [1]                                              from Bubble up shape for Sym(41) <- Sel(Sym(7), Sym(41))
                    // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(41))
                    // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(41))
                    // RuntimeCheck : PRE:    length(S7) = length([u2891])                           from Sel(Sym(7), Sym(41))
                    // RuntimeCheck : PRE:    S41(:length(V7)) < V7                                  from Sel(Sym(7), Sym(41))
                    // Shape: V42=[2] and S42=[]
                    
                    // Shape: V42=[2] and S42=[]
                    val x42: Int = x41.content()(flatten(shape(x41), x7, "sel"))
                    // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(42))
                    // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(42))
                    // RuntimeCheck : POST:   V42 = [2]                                              from Bubble up value for Sym(42) <- Values(Sym(12), Sym(42))
                    // RuntimeCheck : POST:   S42 = []                                               from Bubble up shape for Sym(42) <- Values(Sym(12), Sym(42))
                    // RuntimeCheck : PRE:    S42 = []                                               from Values(Sym(12), Sym(42))
                    // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(42))
                    // Shape: V43=[1  1] and S43=[2]
                    val x43: MDArray[Int] = {
                      val result = new Array[Int](x42)
                      for(i <- List.range(0, result.length))
                      result(i) = x12
                      internalReshape(x42::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(42))
                    // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(42))
                    // RuntimeCheck : POST:   V42 = [2]                                              from Bubble up value for Sym(42) <- Values(Sym(10), Sym(42))
                    // RuntimeCheck : POST:   S42 = []                                               from Bubble up shape for Sym(42) <- Values(Sym(10), Sym(42))
                    // RuntimeCheck : PRE:    S42 = []                                               from Values(Sym(10), Sym(42))
                    // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(42))
                    // Shape: V44=[0  0] and S44=[2]
                    val x44: MDArray[Int] = {
                      val result = new Array[Int](x42)
                      for(i <- List.range(0, result.length))
                      result(i) = x10
                      internalReshape(x42::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V49 = [u777]                                           from Bubble up value for Sym(49) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   V48 = [u786(<3)  u787(<3)]                             from Bubble up value for Sym(48) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S48 = [2]                                              from Bubble up shape for Sym(48) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   V44 = [0  0]                                           from Bubble up value for Sym(44) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S44 = [2]                                              from Bubble up shape for Sym(44) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   V43 = [1  1]                                           from Bubble up value for Sym(43) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S43 = [2]                                              from Bubble up shape for Sym(43) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   V16 = [u116]                                           from Bubble up value for Sym(16) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S16 = []                                               from Bubble up shape for Sym(16) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   V40 = [3  3]                                           from Bubble up value for Sym(40) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S40 = [2]                                              from Bubble up shape for Sym(40) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   V39 = [0  0]                                           from Bubble up value for Sym(39) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : POST:   S39 = [2]                                              from Bubble up shape for Sym(39) <- With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : PRE:    length(S39) = length([u2817])                          from With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : PRE:    S16 = []                                               from With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : PRE:    S40 = S39                                              from With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : PRE:    S43 = S39                                              from With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : PRE:    S44 = S39                                              from With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // RuntimeCheck : PRE:    V39 < V40                                              from With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    // Shape: V50=[u776] and S50=[]
                    // with: With(lb=Sym(39) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(40) step=Sym(43) width=Sym(44)  Sym(48) => Sym(49))
                    val lb0: Int = x39.content()(0)
                    val ub0: Int = x40.content()(0)
                    val step0: Int = x43.content()(0)
                    val width0: Int = x44.content()(0)
                    val ll0: Int = if (x31) lb0 + 1 else lb0
                    val ul0: Int = if (x16) ub0 else ub0 + 1
                    for (iv0 <- List.range(ll0, ul0)) {
                      if ((iv0 - lb0) % step0 <= width0) {
                        val lb1: Int = x39.content()(1)
                        val ub1: Int = x40.content()(1)
                        val step1: Int = x43.content()(1)
                        val width1: Int = x44.content()(1)
                        val ll1: Int = if (x31) lb1 + 1 else lb1
                        val ul1: Int = if (x16) ub1 else ub1 + 1
                        for (iv1 <- List.range(ll1, ul1)) {
                          if ((iv1 - lb1) % step1 <= width1) {
                            val x48: MDArray[Int] = iv0::iv1::Nil
                            val iv: MDArray[Int] = x48
                            val feval: MDArray[Int] = {
                              // RuntimeCheck : POST:   V36 = [u747  u746  u745  u744  u743  u742  u741  u740  u739]     from Bubble up value for Sym(36) <- Sel(Sym(48), Sym(36))
                              // RuntimeCheck : POST:   S36 = [3  3]                                           from Bubble up shape for Sym(36) <- Sel(Sym(48), Sym(36))
                              // RuntimeCheck : POST:   V48 = [u786(<3)  u787(<3)]                             from Bubble up value for Sym(48) <- Sel(Sym(48), Sym(36))
                              // RuntimeCheck : POST:   S48 = [2]                                              from Bubble up shape for Sym(48) <- Sel(Sym(48), Sym(36))
                              // RuntimeCheck : PRE:    length(S48) = length([u2916])                          from Sel(Sym(48), Sym(36))
                              // RuntimeCheck : PRE:    S36(:length(V48)) < V48                                from Sel(Sym(48), Sym(36))
                              // Shape: V49=[u777] and S49=[]
                              
                              // Shape: V49=[u777] and S49=[]
                              val x49: Int = x36.content()(flatten(shape(x36), x48, "sel"))
                              x49
                            }
                            // the action of this loop:
                            result = foldFunction(result, feval)
                          } // if ((iv0 ...
                        } // for (iv0 ...
                      } // if ((iv1 ...
                    } // for (iv1 ...
                    result
                  }
                  
                  // RuntimeCheck : POST:   V51 = [u156]                                           from Bubble up value for Sym(51) <- ToValue(Sym(51))
                  // RuntimeCheck : POST:   S51 = []                                               from Bubble up shape for Sym(51) <- ToValue(Sym(51))
                  // RuntimeCheck : PRE:    length(S51) = length([])                               from ToValue(Sym(51))
                  // Shape: V52=[u155] and S52=[]
                  val x52: Int = x51
                  // RuntimeCheck : POST:   V52 = [u155]                                           from Bubble up value for Sym(52) <- FromValue(Sym(52))
                  // RuntimeCheck : POST:   S52 = []                                               from Bubble up shape for Sym(52) <- FromValue(Sym(52))
                  // Shape: V53=[u154] and S53=[]
                  val x53: Int = x52
                  // RuntimeCheck : POST:   V54 = [u791]                                           from Bubble up value for Sym(54) <- InfixOp(-: Sym(53) and Sym(54))
                  // RuntimeCheck : POST:   S54 = []                                               from Bubble up shape for Sym(54) <- InfixOp(-: Sym(53) and Sym(54))
                  // RuntimeCheck : POST:   V53 = [u154]                                           from Bubble up value for Sym(53) <- InfixOp(-: Sym(53) and Sym(54))
                  // RuntimeCheck : POST:   S53 = []                                               from Bubble up shape for Sym(53) <- InfixOp(-: Sym(53) and Sym(54))
                  // RuntimeCheck : PRE:    S53 = S54 OR S54 = []                                  from InfixOp(-: Sym(53) and Sym(54))
                  // Shape: V57=[u153] and S57=[]
                  val x57: Int = {
                    val result = new Array[Int](shape(x53).content().foldLeft(1)((a,b) => a*b))
                    for(i <- List.range(0, result.length))
                    result(i) = x53.content()(i) -  x54
                    internalReshape(shape(x53), result, "infixOpAA")
                  }
                  // Shape: V58=[2] and S58=[]
                  val x58: Int = internalReshape(Nil, Array(2), "knownAtCompileTime")
                  // RuntimeCheck : POST:   V58 = [2]                                              from Bubble up value for Sym(58) <- InfixOp(<: Sym(57) and Sym(58))
                  // RuntimeCheck : POST:   S58 = []                                               from Bubble up shape for Sym(58) <- InfixOp(<: Sym(57) and Sym(58))
                  // RuntimeCheck : POST:   V57 = [u153]                                           from Bubble up value for Sym(57) <- InfixOp(<: Sym(57) and Sym(58))
                  // RuntimeCheck : POST:   S57 = []                                               from Bubble up shape for Sym(57) <- InfixOp(<: Sym(57) and Sym(58))
                  // RuntimeCheck : PRE:    S57 = S58 OR S58 = []                                  from InfixOp(<: Sym(57) and Sym(58))
                  // Shape: V59=[u152] and S59=[]
                  val x59: Boolean = {
                    val result = new Array[Boolean](shape(x57).content().foldLeft(1)((a,b) => a*b))
                    for(i <- List.range(0, result.length))
                    result(i) = x57.content()(i) <  x58
                    internalReshape(shape(x57), result, "infixOpAA")
                  }
                  // RuntimeCheck : POST:   V59 = [u152]                                           from Bubble up value for Sym(59) <- ToValue(Sym(59))
                  // RuntimeCheck : POST:   S59 = []                                               from Bubble up shape for Sym(59) <- ToValue(Sym(59))
                  // RuntimeCheck : PRE:    length(S59) = length([])                               from ToValue(Sym(59))
                  // Shape: V60=[u146] and S60=[]
                  val x60: Boolean = x59
                  val x66 = if (x60) {
                    x12
                  } else {
                    // RuntimeCheck : POST:   V54 = [u791]                                           from Bubble up value for Sym(54) <- InfixOp(-: Sym(53) and Sym(54))
                    // RuntimeCheck : POST:   S54 = []                                               from Bubble up shape for Sym(54) <- InfixOp(-: Sym(53) and Sym(54))
                    // RuntimeCheck : POST:   V53 = [u154]                                           from Bubble up value for Sym(53) <- InfixOp(-: Sym(53) and Sym(54))
                    // RuntimeCheck : POST:   S53 = []                                               from Bubble up shape for Sym(53) <- InfixOp(-: Sym(53) and Sym(54))
                    // RuntimeCheck : PRE:    S53 = S54 OR S54 = []                                  from InfixOp(-: Sym(53) and Sym(54))
                    // Shape: V61=[u151] and S61=[]
                    val x61: Int = {
                      val result = new Array[Int](shape(x53).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x53.content()(i) -  x54
                      internalReshape(shape(x53), result, "infixOpAA")
                    }
                    // Shape: V62=[4] and S62=[]
                    val x62: Int = internalReshape(Nil, Array(4), "knownAtCompileTime")
                    // RuntimeCheck : POST:   V62 = [4]                                              from Bubble up value for Sym(62) <- InfixOp(<: Sym(61) and Sym(62))
                    // RuntimeCheck : POST:   S62 = []                                               from Bubble up shape for Sym(62) <- InfixOp(<: Sym(61) and Sym(62))
                    // RuntimeCheck : POST:   V61 = [u151]                                           from Bubble up value for Sym(61) <- InfixOp(<: Sym(61) and Sym(62))
                    // RuntimeCheck : POST:   S61 = []                                               from Bubble up shape for Sym(61) <- InfixOp(<: Sym(61) and Sym(62))
                    // RuntimeCheck : PRE:    S61 = S62 OR S62 = []                                  from InfixOp(<: Sym(61) and Sym(62))
                    // Shape: V63=[u150] and S63=[]
                    val x63: Boolean = {
                      val result = new Array[Boolean](shape(x61).content().foldLeft(1)((a,b) => a*b))
                      for(i <- List.range(0, result.length))
                      result(i) = x61.content()(i) <  x62
                      internalReshape(shape(x61), result, "infixOpAA")
                    }
                    // RuntimeCheck : POST:   V63 = [u150]                                           from Bubble up value for Sym(63) <- ToValue(Sym(63))
                    // RuntimeCheck : POST:   S63 = []                                               from Bubble up shape for Sym(63) <- ToValue(Sym(63))
                    // RuntimeCheck : PRE:    length(S63) = length([])                               from ToValue(Sym(63))
                    // Shape: V64=[u142] and S64=[]
                    val x64: Boolean = x63
                    val x65 = if (x64) {
                      x10
                    } else {
                      x12
                    }
                    x65
                  }
                  x66
                } else {
                  x10
                }
                x67
              }
              // the action of this loop:
              if (result == null) {
                // create the array and shape
                result = new Array[Int](x6.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                rshape = shape(feval).content()
              } else {
                // check shape -- this WILL be redundant due to runtime checks
                if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
              }
              // copy new content
              val mainIndex: Int = flatten(x6 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
              for (innerIndex <- List.range(0, rshape.length)) {
                result(mainIndex + innerIndex) = feval(innerIndex)
              }
            } // if ((iv0 ...
          } // for (iv0 ...
        } // if ((iv1 ...
      } // for (iv1 ...
      internalReshape(x6 ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   V6 = [10  10]                                          from Bubble up value for Sym(6) <- GenArrayWith(Sym(6) - Sym(128))
    // RuntimeCheck : POST:   S6 = [2]                                               from Bubble up shape for Sym(6) <- GenArrayWith(Sym(6) - Sym(128))
    // RuntimeCheck : POST:   V128 = [u124]                                          from Bubble up value for Sym(128) <- GenArrayWith(Sym(6) - Sym(128))
    // RuntimeCheck : POST:   S128 = []                                              from Bubble up shape for Sym(128) <- GenArrayWith(Sym(6) - Sym(128))
    // RuntimeCheck : PRE:    S6 = [u1067]                                           from GenArrayWith(Sym(6) - Sym(128))
    // RuntimeCheck : PRE:    S6 = S11                                               from GenArrayWith(Sym(6) - Sym(128))
    // RuntimeCheck : PRE:    V6(:length(V11)) < V11                                 from GenArrayWith(Sym(6) - Sym(128))
    // RuntimeCheck : PRE:    V6(length(V11):) = S128                                from GenArrayWith(Sym(6) - Sym(128))
    // Shape: V129=[u479  u478  u477  ...  u382  u381  u380] and S129=[10  10]
    
    val x129: MDArray[Int] = {
      val opName: String = "genarray"
      var result: Array[Int] = null
      var rshape: Array[Int] = null
      // Shape: V7=[0] and S7=[1]
      val x7: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V6 = [10  10]                                          from Bubble up value for Sym(6) <- Shape(Sym(6))
      // RuntimeCheck : POST:   S6 = [2]                                               from Bubble up shape for Sym(6) <- Shape(Sym(6))
      // Shape: V8=[2] and S8=[1]
      val x8: MDArray[Int] = shape(x6)
      // RuntimeCheck : POST:   V8 = [2]                                               from Bubble up value for Sym(8) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : POST:   S8 = [1]                                               from Bubble up shape for Sym(8) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(8))
      // RuntimeCheck : PRE:    length(S7) = length([u2687])                           from Sel(Sym(7), Sym(8))
      // RuntimeCheck : PRE:    S8(:length(V7)) < V7                                   from Sel(Sym(7), Sym(8))
      // Shape: V9=[2] and S9=[]
      
      // Shape: V9=[2] and S9=[]
      val x9: Int = x8.content()(flatten(shape(x8), x7, "sel"))
      // Shape: V10=[0] and S10=[]
      val x10: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : POST:   V9 = [2]                                               from Bubble up value for Sym(9) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : POST:   S9 = []                                                from Bubble up shape for Sym(9) <- Values(Sym(10), Sym(9))
      // RuntimeCheck : PRE:    S9 = []                                                from Values(Sym(10), Sym(9))
      // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(9))
      // Shape: V11=[0  0] and S11=[2]
      val x11: MDArray[Int] = {
        val result = new Array[Int](x9)
        for(i <- List.range(0, result.length))
        result(i) = x10
        internalReshape(x9::Nil, result, "values")
      }
      // Shape: V12=[1] and S12=[]
      val x12: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : POST:   V9 = [2]                                               from Bubble up value for Sym(9) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : POST:   S9 = []                                                from Bubble up shape for Sym(9) <- Values(Sym(12), Sym(9))
      // RuntimeCheck : PRE:    S9 = []                                                from Values(Sym(12), Sym(9))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(9))
      // Shape: V14=[1  1] and S14=[2]
      val x14: MDArray[Int] = {
        val result = new Array[Int](x9)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x9::Nil, result, "values")
      }
      // Shape: V4=[u5] and S4=[]
      val x4: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V4 = [u5]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
      // Shape: V5=[u118] and S5=[]
      val x5: Boolean = x4
      // RuntimeCheck : POST:   V5 = [u118]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
      // Shape: V15=[u117] and S15=[]
      val x15: Boolean = x5
      // RuntimeCheck : POST:   V15 = [u117]                                           from Bubble up value for Sym(15) <- ToValue(Sym(15))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- ToValue(Sym(15))
      // RuntimeCheck : PRE:    length(S15) = length([])                               from ToValue(Sym(15))
      // Shape: V16=[u116] and S16=[]
      val x16: Boolean = x15
      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : POST:   V6 = [10  10]                                          from Bubble up value for Sym(6) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : POST:   S6 = [2]                                               from Bubble up shape for Sym(6) <- InfixOp(-: Sym(6) and Sym(12))
      // RuntimeCheck : PRE:    S6 = S12 OR S12 = []                                   from InfixOp(-: Sym(6) and Sym(12))
      // Shape: V70=[u177  u176] and S70=[2]
      val x70: MDArray[Int] = {
        val result = new Array[Int](shape(x6).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x6.content()(i) -  x12
        internalReshape(shape(x6), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V127 = [u125]                                          from Bubble up value for Sym(127) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S127 = []                                              from Bubble up shape for Sym(127) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   V71 = [u774(<10)  u775(<10)]                           from Bubble up value for Sym(71) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S71 = [2]                                              from Bubble up shape for Sym(71) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   V11 = [0  0]                                           from Bubble up value for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S11 = [2]                                              from Bubble up shape for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   V14 = [1  1]                                           from Bubble up value for Sym(14) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S14 = [2]                                              from Bubble up shape for Sym(14) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   V16 = [u116]                                           from Bubble up value for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S16 = []                                               from Bubble up shape for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   V70 = [u177  u176]                                     from Bubble up value for Sym(70) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S70 = [2]                                              from Bubble up shape for Sym(70) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   V16 = [u116]                                           from Bubble up value for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S16 = []                                               from Bubble up shape for Sym(16) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   V11 = [0  0]                                           from Bubble up value for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : POST:   S11 = [2]                                              from Bubble up shape for Sym(11) <- With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : PRE:    length(S11) = length([u1068])                          from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : PRE:    S16 = []                                               from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : PRE:    S16 = []                                               from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : PRE:    S70 = S11                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : PRE:    S14 = S11                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : PRE:    S11 = S11                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // RuntimeCheck : PRE:    V11 < V70                                              from With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      // Shape: V128=[u124] and S128=[]
      // with: With(lb=Sym(11) lbStrict=Sym(16) ubStict=Sym(16) ub=Sym(70) step=Sym(14) width=Sym(11)  Sym(71) => Sym(127))
      val lb0: Int = x11.content()(0)
      val ub0: Int = x70.content()(0)
      val step0: Int = x14.content()(0)
      val width0: Int = x11.content()(0)
      val ll0: Int = if (x16) lb0 + 1 else lb0
      val ul0: Int = if (x16) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x11.content()(1)
          val ub1: Int = x70.content()(1)
          val step1: Int = x14.content()(1)
          val width1: Int = x11.content()(1)
          val ll1: Int = if (x16) lb1 + 1 else lb1
          val ul1: Int = if (x16) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val x71: MDArray[Int] = iv0::iv1::Nil
              val iv: MDArray[Int] = x71
              val feval: MDArray[Int] = {
                // RuntimeCheck : POST:   V69 = [u579  u578  u577  u576  u575  u574  u573  u572  u571  u570  u569  u568  u567  u566  u565  u564  u563  u562  u561  u560  u559  u558  u557  u556  u555  u554  u553  u552  u551  u550  u549  u548  u547  u546  u545  u544  u543  u542  u541  u540  u539  u538  u537  u536  u535  u534  u533  u532  u531  u530  u529  u528  u527  u526  u525  u524  u523  u522  u521  u520  u519  u518  u517  u516  u515  u514  u513  u512  u511  u510  u509  u508  u507  u506  u505  u504  u503  u502  u501  u500  u499  u498  u497  u496  u495  u494  u493  u492  u491  u490  u489  u488  u487  u486  u485  u484  u483  u482  u481  u480]     from Bubble up value for Sym(69) <- Sel(Sym(71), Sym(69))
                // RuntimeCheck : POST:   S69 = [10  10]                                         from Bubble up shape for Sym(69) <- Sel(Sym(71), Sym(69))
                // RuntimeCheck : POST:   V71 = [u774(<10)  u775(<10)]                           from Bubble up value for Sym(71) <- Sel(Sym(71), Sym(69))
                // RuntimeCheck : POST:   S71 = [2]                                              from Bubble up shape for Sym(71) <- Sel(Sym(71), Sym(69))
                // RuntimeCheck : PRE:    length(S71) = length([u1085])                          from Sel(Sym(71), Sym(69))
                // RuntimeCheck : PRE:    S69(:length(V71)) < V71                                from Sel(Sym(71), Sym(69))
                // Shape: V121=[u771] and S121=[]
                
                // Shape: V121=[u771] and S121=[]
                val x121: Int = x69.content()(flatten(shape(x69), x71, "sel"))
                // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(===: Sym(121) and Sym(12))
                // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(===: Sym(121) and Sym(12))
                // RuntimeCheck : POST:   V121 = [u771]                                          from Bubble up value for Sym(121) <- InfixOp(===: Sym(121) and Sym(12))
                // RuntimeCheck : POST:   S121 = []                                              from Bubble up shape for Sym(121) <- InfixOp(===: Sym(121) and Sym(12))
                // RuntimeCheck : PRE:    S121 = S12 OR S12 = []                                 from InfixOp(===: Sym(121) and Sym(12))
                // Shape: V122=[u770] and S122=[]
                val x122: Boolean = {
                  val result = new Array[Boolean](shape(x121).content().foldLeft(1)((a,b) => a*b))
                  for(i <- List.range(0, result.length))
                  result(i) = x121.content()(i) ===  x12
                  internalReshape(shape(x121), result, "infixOpAA")
                }
                // RuntimeCheck : POST:   V122 = [u770]                                          from Bubble up value for Sym(122) <- ToValue(Sym(122))
                // RuntimeCheck : POST:   S122 = []                                              from Bubble up shape for Sym(122) <- ToValue(Sym(122))
                // RuntimeCheck : PRE:    length(S122) = length([])                              from ToValue(Sym(122))
                // Shape: V123=[u113] and S123=[]
                val x123: Boolean = x122
                val x127 = if (x123) {
                  // Shape: V20=[3] and S20=[]
                  val x20: Int = internalReshape(Nil, Array(3), "knownAtCompileTime")
                  // RuntimeCheck : POST:   V89 = [u98]                                            from Bubble up value for Sym(89) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : POST:   S89 = []                                               from Bubble up shape for Sym(89) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : POST:   S88 = []                                               from Bubble up shape for Sym(88) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : POST:   S87 = []                                               from Bubble up shape for Sym(87) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : POST:   V92 = [u756]                                           from Bubble up value for Sym(92) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : POST:   S92 = []                                               from Bubble up shape for Sym(92) <- FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : PRE:    S10 = S92                                              from FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // RuntimeCheck : PRE:    S89 = S92                                              from FoldArrayWith(Sym(10), fold (Sym(87), Sym(88)) => Sym(89), Sym(92))
                  // Shape: V93=[u128] and S93=[]
                  
                  val x93: Int = {
                    val opName: String = "fold"
                    var result: MDArray[Int] = x10
                    val foldFunction: (MDArray[Int], MDArray[Int]) => MDArray[Int] = (x87, x88) => {
                      // RuntimeCheck : POST:   S88 = []                                               from Bubble up shape for Sym(88) <- ScalarOperator Sym(87) + Sym(88)
                      // RuntimeCheck : POST:   S87 = []                                               from Bubble up shape for Sym(87) <- ScalarOperator Sym(87) + Sym(88)
                      // RuntimeCheck : PRE:    S87 = []                                               from ScalarOperator Sym(87) + Sym(88)
                      // RuntimeCheck : PRE:    S88 = []                                               from ScalarOperator Sym(87) + Sym(88)
                      // Shape: V89=[u98] and S89=[]
                      val x89: Int = ((a: Int, b: Int) => a + b)(x87, x88)
                      x89
                    }
                    // Shape: V23=[u7] and S23=[]
                    val x23: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
                    // RuntimeCheck : POST:   V23 = [u7]                                             from Bubble up value for Sym(23) <- ToValue(Sym(23))
                    // RuntimeCheck : POST:   S23 = []                                               from Bubble up shape for Sym(23) <- ToValue(Sym(23))
                    // RuntimeCheck : PRE:    length(S23) = length([])                               from ToValue(Sym(23))
                    // Shape: V24=[u89] and S24=[]
                    val x24: Boolean = x23
                    // RuntimeCheck : POST:   V24 = [u89]                                            from Bubble up value for Sym(24) <- FromValue(Sym(24))
                    // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- FromValue(Sym(24))
                    // Shape: V30=[u88] and S30=[]
                    val x30: Boolean = x24
                    // RuntimeCheck : POST:   V30 = [u88]                                            from Bubble up value for Sym(30) <- ToValue(Sym(30))
                    // RuntimeCheck : POST:   S30 = []                                               from Bubble up shape for Sym(30) <- ToValue(Sym(30))
                    // RuntimeCheck : PRE:    length(S30) = length([])                               from ToValue(Sym(30))
                    // Shape: V31=[u87] and S31=[]
                    val x31: Boolean = x30
                    // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- Dim(Sym(3))
                    // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- Dim(Sym(3))
                    // Shape: V18=[2] and S18=[]
                    val x18: Int = dim(x3)
                    // RuntimeCheck : POST:   V18 = [2]                                              from Bubble up value for Sym(18) <- FromValue(Sym(18))
                    // RuntimeCheck : POST:   S18 = []                                               from Bubble up shape for Sym(18) <- FromValue(Sym(18))
                    // Shape: V19=[2] and S19=[]
                    val x19: Int = x18
                    // RuntimeCheck : POST:   V20 = [3]                                              from Bubble up value for Sym(20) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   V19 = [2]                                              from Bubble up value for Sym(19) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   S19 = []                                               from Bubble up shape for Sym(19) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : PRE:    S19 = []                                               from Values(Sym(20), Sym(19))
                    // RuntimeCheck : PRE:    S20 = []                                               from Values(Sym(20), Sym(19))
                    // Shape: V21=[3  3] and S21=[2]
                    val x21: MDArray[Int] = {
                      val result = new Array[Int](x19)
                      for(i <- List.range(0, result.length))
                      result(i) = x20
                      internalReshape(x19::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- GenArrayWith(Sym(21) - Sym(77))
                    // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- GenArrayWith(Sym(21) - Sym(77))
                    // RuntimeCheck : POST:   V77 = [u733]                                           from Bubble up value for Sym(77) <- GenArrayWith(Sym(21) - Sym(77))
                    // RuntimeCheck : POST:   S77 = []                                               from Bubble up shape for Sym(77) <- GenArrayWith(Sym(21) - Sym(77))
                    // RuntimeCheck : PRE:    S21 = [u1456]                                          from GenArrayWith(Sym(21) - Sym(77))
                    // RuntimeCheck : PRE:    S21 = S27                                              from GenArrayWith(Sym(21) - Sym(77))
                    // RuntimeCheck : PRE:    V21(:length(V27)) < V27                                from GenArrayWith(Sym(21) - Sym(77))
                    // RuntimeCheck : PRE:    V21(length(V27):) = S77                                from GenArrayWith(Sym(21) - Sym(77))
                    // Shape: V78=[u732  u731  u730  ...  u726  u725  u724] and S78=[3  3]
                    
                    val x78: MDArray[Int] = {
                      val opName: String = "genarray"
                      var result: Array[Int] = null
                      var rshape: Array[Int] = null
                      // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- Shape(Sym(21))
                      // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- Shape(Sym(21))
                      // Shape: V25=[2] and S25=[1]
                      val x25: MDArray[Int] = shape(x21)
                      // RuntimeCheck : POST:   V25 = [2]                                              from Bubble up value for Sym(25) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   S25 = [1]                                              from Bubble up shape for Sym(25) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : PRE:    length(S7) = length([u2931])                           from Sel(Sym(7), Sym(25))
                      // RuntimeCheck : PRE:    S25(:length(V7)) < V7                                  from Sel(Sym(7), Sym(25))
                      // Shape: V26=[2] and S26=[]
                      
                      // Shape: V26=[2] and S26=[]
                      val x26: Int = x25.content()(flatten(shape(x25), x7, "sel"))
                      // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   V26 = [2]                                              from Bubble up value for Sym(26) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   S26 = []                                               from Bubble up shape for Sym(26) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : PRE:    S26 = []                                               from Values(Sym(10), Sym(26))
                      // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(26))
                      // Shape: V27=[0  0] and S27=[2]
                      val x27: MDArray[Int] = {
                        val result = new Array[Int](x26)
                        for(i <- List.range(0, result.length))
                        result(i) = x10
                        internalReshape(x26::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   V26 = [2]                                              from Bubble up value for Sym(26) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   S26 = []                                               from Bubble up shape for Sym(26) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : PRE:    S26 = []                                               from Values(Sym(12), Sym(26))
                      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(26))
                      // Shape: V29=[1  1] and S29=[2]
                      val x29: MDArray[Int] = {
                        val result = new Array[Int](x26)
                        for(i <- List.range(0, result.length))
                        result(i) = x12
                        internalReshape(x26::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : PRE:    S21 = S12 OR S12 = []                                  from InfixOp(-: Sym(21) and Sym(12))
                      // Shape: V73=[u736  u735] and S73=[2]
                      val x73: MDArray[Int] = {
                        val result = new Array[Int](shape(x21).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x21.content()(i) -  x12
                        internalReshape(shape(x21), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : POST:   V71 = [u774(<10)  u775(<10)]                           from Bubble up value for Sym(71) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : POST:   S71 = [2]                                              from Bubble up shape for Sym(71) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : PRE:    S71 = S12 OR S12 = []                                  from InfixOp(-: Sym(71) and Sym(12))
                      // Shape: V72=[u683  u682] and S72=[2]
                      val x72: MDArray[Int] = {
                        val result = new Array[Int](shape(x71).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x71.content()(i) -  x12
                        internalReshape(shape(x71), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V76 = [u734]                                           from Bubble up value for Sym(76) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S76 = []                                               from Bubble up shape for Sym(76) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S74 = [2]                                              from Bubble up shape for Sym(74) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   V27 = [0  0]                                           from Bubble up value for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S27 = [2]                                              from Bubble up shape for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   V29 = [1  1]                                           from Bubble up value for Sym(29) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S29 = [2]                                              from Bubble up shape for Sym(29) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   V73 = [u736  u735]                                     from Bubble up value for Sym(73) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S73 = [2]                                              from Bubble up shape for Sym(73) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   V27 = [0  0]                                           from Bubble up value for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : POST:   S27 = [2]                                              from Bubble up shape for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : PRE:    length(S27) = length([u1457])                          from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : PRE:    S73 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : PRE:    S29 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : PRE:    S27 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // RuntimeCheck : PRE:    V27 < V73                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      // Shape: V77=[u733] and S77=[]
                      // with: With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(73) step=Sym(29) width=Sym(27)  Sym(74) => Sym(76))
                      val lb0: Int = x27.content()(0)
                      val ub0: Int = x73.content()(0)
                      val step0: Int = x29.content()(0)
                      val width0: Int = x27.content()(0)
                      val ll0: Int = if (x31) lb0 + 1 else lb0
                      val ul0: Int = if (x31) ub0 else ub0 + 1
                      for (iv0 <- List.range(ll0, ul0)) {
                        if ((iv0 - lb0) % step0 <= width0) {
                          val lb1: Int = x27.content()(1)
                          val ub1: Int = x73.content()(1)
                          val step1: Int = x29.content()(1)
                          val width1: Int = x27.content()(1)
                          val ll1: Int = if (x31) lb1 + 1 else lb1
                          val ul1: Int = if (x31) ub1 else ub1 + 1
                          for (iv1 <- List.range(ll1, ul1)) {
                            if ((iv1 - lb1) % step1 <= width1) {
                              val x74: MDArray[Int] = iv0::iv1::Nil
                              val iv: MDArray[Int] = x74
                              val feval: MDArray[Int] = {
                                // RuntimeCheck : POST:   V72 = [u683  u682]                                     from Bubble up value for Sym(72) <- InfixOp(+: Sym(74) and Sym(72))
                                // RuntimeCheck : POST:   S72 = [2]                                              from Bubble up shape for Sym(72) <- InfixOp(+: Sym(74) and Sym(72))
                                // RuntimeCheck : POST:   S74 = [2]                                              from Bubble up shape for Sym(74) <- InfixOp(+: Sym(74) and Sym(72))
                                // RuntimeCheck : PRE:    S74 = S72 OR S72 = []                                  from InfixOp(+: Sym(74) and Sym(72))
                                // Shape: V75=[u768(<10)  u769(<10)] and S75=[2]
                                val x75: MDArray[Int] = {
                                  val result = new Array[Int](shape(x74).content().foldLeft(1)((a,b) => a*b))
                                  for(i <- List.range(0, result.length))
                                  result(i) = x74.content()(i) +  x72.content()(i)
                                  internalReshape(shape(x74), result, "infixOpAA")
                                }
                                // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- Sel(Sym(75), Sym(3))
                                // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- Sel(Sym(75), Sym(3))
                                // RuntimeCheck : POST:   V75 = [u768(<10)  u769(<10)]                           from Bubble up value for Sym(75) <- Sel(Sym(75), Sym(3))
                                // RuntimeCheck : POST:   S75 = [2]                                              from Bubble up shape for Sym(75) <- Sel(Sym(75), Sym(3))
                                // RuntimeCheck : PRE:    length(S75) = length([u1474])                          from Sel(Sym(75), Sym(3))
                                // RuntimeCheck : PRE:    S3(:length(V75)) < V75                                 from Sel(Sym(75), Sym(3))
                                // Shape: V76=[u734] and S76=[]
                                
                                // Shape: V76=[u734] and S76=[]
                                val x76: Int = x3.content()(flatten(shape(x3), x75, "sel"))
                                x76
                              }
                              // the action of this loop:
                              if (result == null) {
                                // create the array and shape
                                result = new Array[Int](x21.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                rshape = shape(feval).content()
                              } else {
                                // check shape -- this WILL be redundant due to runtime checks
                                if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                              }
                              // copy new content
                              val mainIndex: Int = flatten(x21 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                              for (innerIndex <- List.range(0, rshape.length)) {
                                result(mainIndex + innerIndex) = feval(innerIndex)
                              }
                            } // if ((iv0 ...
                          } // for (iv0 ...
                        } // if ((iv1 ...
                      } // for (iv1 ...
                      internalReshape(x21 ::: rshape.toList, result, opName)
                    }
                    
                    // RuntimeCheck : POST:   V78 = [u732  u731  u730  u729  u728  u727  u726  u725  u724]     from Bubble up value for Sym(78) <- Dim(Sym(78))
                    // RuntimeCheck : POST:   S78 = [3  3]                                           from Bubble up shape for Sym(78) <- Dim(Sym(78))
                    // Shape: V79=[2] and S79=[]
                    val x79: Int = dim(x78)
                    // RuntimeCheck : POST:   V79 = [2]                                              from Bubble up value for Sym(79) <- FromValue(Sym(79))
                    // RuntimeCheck : POST:   S79 = []                                               from Bubble up shape for Sym(79) <- FromValue(Sym(79))
                    // Shape: V80=[2] and S80=[]
                    val x80: Int = x79
                    // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(80))
                    // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(80))
                    // RuntimeCheck : POST:   V80 = [2]                                              from Bubble up value for Sym(80) <- Values(Sym(10), Sym(80))
                    // RuntimeCheck : POST:   S80 = []                                               from Bubble up shape for Sym(80) <- Values(Sym(10), Sym(80))
                    // RuntimeCheck : PRE:    S80 = []                                               from Values(Sym(10), Sym(80))
                    // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(80))
                    // Shape: V81=[0  0] and S81=[2]
                    val x81: MDArray[Int] = {
                      val result = new Array[Int](x80)
                      for(i <- List.range(0, result.length))
                      result(i) = x10
                      internalReshape(x80::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V78 = [u732  u731  u730  u729  u728  u727  u726  u725  u724]     from Bubble up value for Sym(78) <- Shape(Sym(78))
                    // RuntimeCheck : POST:   S78 = [3  3]                                           from Bubble up shape for Sym(78) <- Shape(Sym(78))
                    // Shape: V82=[3  3] and S82=[2]
                    val x82: MDArray[Int] = shape(x78)
                    // RuntimeCheck : POST:   V81 = [0  0]                                           from Bubble up value for Sym(81) <- Shape(Sym(81))
                    // RuntimeCheck : POST:   S81 = [2]                                              from Bubble up shape for Sym(81) <- Shape(Sym(81))
                    // Shape: V83=[2] and S83=[1]
                    val x83: MDArray[Int] = shape(x81)
                    // RuntimeCheck : POST:   V83 = [2]                                              from Bubble up value for Sym(83) <- Sel(Sym(7), Sym(83))
                    // RuntimeCheck : POST:   S83 = [1]                                              from Bubble up shape for Sym(83) <- Sel(Sym(7), Sym(83))
                    // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(83))
                    // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(83))
                    // RuntimeCheck : PRE:    length(S7) = length([u1430])                           from Sel(Sym(7), Sym(83))
                    // RuntimeCheck : PRE:    S83(:length(V7)) < V7                                  from Sel(Sym(7), Sym(83))
                    // Shape: V84=[2] and S84=[]
                    
                    // Shape: V84=[2] and S84=[]
                    val x84: Int = x83.content()(flatten(shape(x83), x7, "sel"))
                    // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(84))
                    // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(84))
                    // RuntimeCheck : POST:   V84 = [2]                                              from Bubble up value for Sym(84) <- Values(Sym(12), Sym(84))
                    // RuntimeCheck : POST:   S84 = []                                               from Bubble up shape for Sym(84) <- Values(Sym(12), Sym(84))
                    // RuntimeCheck : PRE:    S84 = []                                               from Values(Sym(12), Sym(84))
                    // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(84))
                    // Shape: V85=[1  1] and S85=[2]
                    val x85: MDArray[Int] = {
                      val result = new Array[Int](x84)
                      for(i <- List.range(0, result.length))
                      result(i) = x12
                      internalReshape(x84::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(84))
                    // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(84))
                    // RuntimeCheck : POST:   V84 = [2]                                              from Bubble up value for Sym(84) <- Values(Sym(10), Sym(84))
                    // RuntimeCheck : POST:   S84 = []                                               from Bubble up shape for Sym(84) <- Values(Sym(10), Sym(84))
                    // RuntimeCheck : PRE:    S84 = []                                               from Values(Sym(10), Sym(84))
                    // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(84))
                    // Shape: V86=[0  0] and S86=[2]
                    val x86: MDArray[Int] = {
                      val result = new Array[Int](x84)
                      for(i <- List.range(0, result.length))
                      result(i) = x10
                      internalReshape(x84::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V91 = [u757]                                           from Bubble up value for Sym(91) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S91 = []                                               from Bubble up shape for Sym(91) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   V90 = [u766(<3)  u767(<3)]                             from Bubble up value for Sym(90) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S90 = [2]                                              from Bubble up shape for Sym(90) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   V86 = [0  0]                                           from Bubble up value for Sym(86) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S86 = [2]                                              from Bubble up shape for Sym(86) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   V85 = [1  1]                                           from Bubble up value for Sym(85) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S85 = [2]                                              from Bubble up shape for Sym(85) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   V16 = [u116]                                           from Bubble up value for Sym(16) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S16 = []                                               from Bubble up shape for Sym(16) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   V82 = [3  3]                                           from Bubble up value for Sym(82) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S82 = [2]                                              from Bubble up shape for Sym(82) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   V81 = [0  0]                                           from Bubble up value for Sym(81) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : POST:   S81 = [2]                                              from Bubble up shape for Sym(81) <- With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : PRE:    length(S81) = length([u1356])                          from With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : PRE:    S16 = []                                               from With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : PRE:    S82 = S81                                              from With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : PRE:    S85 = S81                                              from With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : PRE:    S86 = S81                                              from With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // RuntimeCheck : PRE:    V81 < V82                                              from With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    // Shape: V92=[u756] and S92=[]
                    // with: With(lb=Sym(81) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(82) step=Sym(85) width=Sym(86)  Sym(90) => Sym(91))
                    val lb0: Int = x81.content()(0)
                    val ub0: Int = x82.content()(0)
                    val step0: Int = x85.content()(0)
                    val width0: Int = x86.content()(0)
                    val ll0: Int = if (x31) lb0 + 1 else lb0
                    val ul0: Int = if (x16) ub0 else ub0 + 1
                    for (iv0 <- List.range(ll0, ul0)) {
                      if ((iv0 - lb0) % step0 <= width0) {
                        val lb1: Int = x81.content()(1)
                        val ub1: Int = x82.content()(1)
                        val step1: Int = x85.content()(1)
                        val width1: Int = x86.content()(1)
                        val ll1: Int = if (x31) lb1 + 1 else lb1
                        val ul1: Int = if (x16) ub1 else ub1 + 1
                        for (iv1 <- List.range(ll1, ul1)) {
                          if ((iv1 - lb1) % step1 <= width1) {
                            val x90: MDArray[Int] = iv0::iv1::Nil
                            val iv: MDArray[Int] = x90
                            val feval: MDArray[Int] = {
                              // RuntimeCheck : POST:   V78 = [u732  u731  u730  u729  u728  u727  u726  u725  u724]     from Bubble up value for Sym(78) <- Sel(Sym(90), Sym(78))
                              // RuntimeCheck : POST:   S78 = [3  3]                                           from Bubble up shape for Sym(78) <- Sel(Sym(90), Sym(78))
                              // RuntimeCheck : POST:   V90 = [u766(<3)  u767(<3)]                             from Bubble up value for Sym(90) <- Sel(Sym(90), Sym(78))
                              // RuntimeCheck : POST:   S90 = [2]                                              from Bubble up shape for Sym(90) <- Sel(Sym(90), Sym(78))
                              // RuntimeCheck : PRE:    length(S90) = length([u1455])                          from Sel(Sym(90), Sym(78))
                              // RuntimeCheck : PRE:    S78(:length(V90)) < V90                                from Sel(Sym(90), Sym(78))
                              // Shape: V91=[u757] and S91=[]
                              
                              // Shape: V91=[u757] and S91=[]
                              val x91: Int = x78.content()(flatten(shape(x78), x90, "sel"))
                              x91
                            }
                            // the action of this loop:
                            result = foldFunction(result, feval)
                          } // if ((iv0 ...
                        } // for (iv0 ...
                      } // if ((iv1 ...
                    } // for (iv1 ...
                    result
                  }
                  
                  // RuntimeCheck : POST:   V93 = [u128]                                           from Bubble up value for Sym(93) <- ToValue(Sym(93))
                  // RuntimeCheck : POST:   S93 = []                                               from Bubble up shape for Sym(93) <- ToValue(Sym(93))
                  // RuntimeCheck : PRE:    length(S93) = length([])                               from ToValue(Sym(93))
                  // Shape: V94=[u94] and S94=[]
                  val x94: Int = x93
                  // RuntimeCheck : POST:   V94 = [u94]                                            from Bubble up value for Sym(94) <- FromValue(Sym(94))
                  // RuntimeCheck : POST:   S94 = []                                               from Bubble up shape for Sym(94) <- FromValue(Sym(94))
                  // Shape: V95=[u93] and S95=[]
                  val x95: Int = x94
                  // RuntimeCheck : POST:   V113 = [u66]                                           from Bubble up value for Sym(113) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : POST:   S113 = []                                              from Bubble up shape for Sym(113) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : POST:   S112 = []                                              from Bubble up shape for Sym(112) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : POST:   S111 = []                                              from Bubble up shape for Sym(111) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : POST:   V116 = [u690]                                          from Bubble up value for Sym(116) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : POST:   S116 = []                                              from Bubble up shape for Sym(116) <- FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : PRE:    S10 = S116                                             from FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // RuntimeCheck : PRE:    S113 = S116                                            from FoldArrayWith(Sym(10), fold (Sym(111), Sym(112)) => Sym(113), Sym(116))
                  // Shape: V117=[u127] and S117=[]
                  
                  val x117: Int = {
                    val opName: String = "fold"
                    var result: MDArray[Int] = x10
                    val foldFunction: (MDArray[Int], MDArray[Int]) => MDArray[Int] = (x111, x112) => {
                      // RuntimeCheck : POST:   S112 = []                                              from Bubble up shape for Sym(112) <- ScalarOperator Sym(111) + Sym(112)
                      // RuntimeCheck : POST:   S111 = []                                              from Bubble up shape for Sym(111) <- ScalarOperator Sym(111) + Sym(112)
                      // RuntimeCheck : PRE:    S111 = []                                              from ScalarOperator Sym(111) + Sym(112)
                      // RuntimeCheck : PRE:    S112 = []                                              from ScalarOperator Sym(111) + Sym(112)
                      // Shape: V113=[u66] and S113=[]
                      val x113: Int = ((a: Int, b: Int) => a + b)(x111, x112)
                      x113
                    }
                    // Shape: V23=[u7] and S23=[]
                    val x23: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
                    // RuntimeCheck : POST:   V23 = [u7]                                             from Bubble up value for Sym(23) <- ToValue(Sym(23))
                    // RuntimeCheck : POST:   S23 = []                                               from Bubble up shape for Sym(23) <- ToValue(Sym(23))
                    // RuntimeCheck : PRE:    length(S23) = length([])                               from ToValue(Sym(23))
                    // Shape: V24=[u89] and S24=[]
                    val x24: Boolean = x23
                    // RuntimeCheck : POST:   V24 = [u89]                                            from Bubble up value for Sym(24) <- FromValue(Sym(24))
                    // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- FromValue(Sym(24))
                    // Shape: V30=[u88] and S30=[]
                    val x30: Boolean = x24
                    // RuntimeCheck : POST:   V30 = [u88]                                            from Bubble up value for Sym(30) <- ToValue(Sym(30))
                    // RuntimeCheck : POST:   S30 = []                                               from Bubble up shape for Sym(30) <- ToValue(Sym(30))
                    // RuntimeCheck : PRE:    length(S30) = length([])                               from ToValue(Sym(30))
                    // Shape: V31=[u87] and S31=[]
                    val x31: Boolean = x30
                    // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- Dim(Sym(3))
                    // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- Dim(Sym(3))
                    // Shape: V18=[2] and S18=[]
                    val x18: Int = dim(x3)
                    // RuntimeCheck : POST:   V18 = [2]                                              from Bubble up value for Sym(18) <- FromValue(Sym(18))
                    // RuntimeCheck : POST:   S18 = []                                               from Bubble up shape for Sym(18) <- FromValue(Sym(18))
                    // Shape: V19=[2] and S19=[]
                    val x19: Int = x18
                    // RuntimeCheck : POST:   V20 = [3]                                              from Bubble up value for Sym(20) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   V19 = [2]                                              from Bubble up value for Sym(19) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : POST:   S19 = []                                               from Bubble up shape for Sym(19) <- Values(Sym(20), Sym(19))
                    // RuntimeCheck : PRE:    S19 = []                                               from Values(Sym(20), Sym(19))
                    // RuntimeCheck : PRE:    S20 = []                                               from Values(Sym(20), Sym(19))
                    // Shape: V21=[3  3] and S21=[2]
                    val x21: MDArray[Int] = {
                      val result = new Array[Int](x19)
                      for(i <- List.range(0, result.length))
                      result(i) = x20
                      internalReshape(x19::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- GenArrayWith(Sym(21) - Sym(101))
                    // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- GenArrayWith(Sym(21) - Sym(101))
                    // RuntimeCheck : POST:   V101 = [u695]                                          from Bubble up value for Sym(101) <- GenArrayWith(Sym(21) - Sym(101))
                    // RuntimeCheck : POST:   S101 = []                                              from Bubble up shape for Sym(101) <- GenArrayWith(Sym(21) - Sym(101))
                    // RuntimeCheck : PRE:    S21 = [u2654]                                          from GenArrayWith(Sym(21) - Sym(101))
                    // RuntimeCheck : PRE:    S21 = S27                                              from GenArrayWith(Sym(21) - Sym(101))
                    // RuntimeCheck : PRE:    V21(:length(V27)) < V27                                from GenArrayWith(Sym(21) - Sym(101))
                    // RuntimeCheck : PRE:    V21(length(V27):) = S101                               from GenArrayWith(Sym(21) - Sym(101))
                    // Shape: V102=[u721  u720  u719  ...  u715  u714  u713] and S102=[3  3]
                    
                    val x102: MDArray[Int] = {
                      val opName: String = "genarray"
                      var result: Array[Int] = null
                      var rshape: Array[Int] = null
                      // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- Shape(Sym(21))
                      // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- Shape(Sym(21))
                      // Shape: V25=[2] and S25=[1]
                      val x25: MDArray[Int] = shape(x21)
                      // RuntimeCheck : POST:   V25 = [2]                                              from Bubble up value for Sym(25) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   S25 = [1]                                              from Bubble up shape for Sym(25) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(25))
                      // RuntimeCheck : PRE:    length(S7) = length([u2931])                           from Sel(Sym(7), Sym(25))
                      // RuntimeCheck : PRE:    S25(:length(V7)) < V7                                  from Sel(Sym(7), Sym(25))
                      // Shape: V26=[2] and S26=[]
                      
                      // Shape: V26=[2] and S26=[]
                      val x26: Int = x25.content()(flatten(shape(x25), x7, "sel"))
                      // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   V26 = [2]                                              from Bubble up value for Sym(26) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : POST:   S26 = []                                               from Bubble up shape for Sym(26) <- Values(Sym(10), Sym(26))
                      // RuntimeCheck : PRE:    S26 = []                                               from Values(Sym(10), Sym(26))
                      // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(26))
                      // Shape: V27=[0  0] and S27=[2]
                      val x27: MDArray[Int] = {
                        val result = new Array[Int](x26)
                        for(i <- List.range(0, result.length))
                        result(i) = x10
                        internalReshape(x26::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   V26 = [2]                                              from Bubble up value for Sym(26) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : POST:   S26 = []                                               from Bubble up shape for Sym(26) <- Values(Sym(12), Sym(26))
                      // RuntimeCheck : PRE:    S26 = []                                               from Values(Sym(12), Sym(26))
                      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(26))
                      // Shape: V29=[1  1] and S29=[2]
                      val x29: MDArray[Int] = {
                        val result = new Array[Int](x26)
                        for(i <- List.range(0, result.length))
                        result(i) = x12
                        internalReshape(x26::Nil, result, "values")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   V21 = [3  3]                                           from Bubble up value for Sym(21) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : POST:   S21 = [2]                                              from Bubble up shape for Sym(21) <- InfixOp(-: Sym(21) and Sym(12))
                      // RuntimeCheck : PRE:    S21 = S12 OR S12 = []                                  from InfixOp(-: Sym(21) and Sym(12))
                      // Shape: V97=[u698  u697] and S97=[2]
                      val x97: MDArray[Int] = {
                        val result = new Array[Int](shape(x21).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x21.content()(i) -  x12
                        internalReshape(shape(x21), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : POST:   V71 = [u774(<10)  u775(<10)]                           from Bubble up value for Sym(71) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : POST:   S71 = [2]                                              from Bubble up shape for Sym(71) <- InfixOp(-: Sym(71) and Sym(12))
                      // RuntimeCheck : PRE:    S71 = S12 OR S12 = []                                  from InfixOp(-: Sym(71) and Sym(12))
                      // Shape: V96=[u681  u680] and S96=[2]
                      val x96: MDArray[Int] = {
                        val result = new Array[Int](shape(x71).content().foldLeft(1)((a,b) => a*b))
                        for(i <- List.range(0, result.length))
                        result(i) = x71.content()(i) -  x12
                        internalReshape(shape(x71), result, "infixOpAA")
                      }
                      // RuntimeCheck : POST:   V100 = [u696]                                          from Bubble up value for Sym(100) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S100 = []                                              from Bubble up shape for Sym(100) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S98 = [2]                                              from Bubble up shape for Sym(98) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   V27 = [0  0]                                           from Bubble up value for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S27 = [2]                                              from Bubble up shape for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   V29 = [1  1]                                           from Bubble up value for Sym(29) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S29 = [2]                                              from Bubble up shape for Sym(29) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   V97 = [u698  u697]                                     from Bubble up value for Sym(97) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S97 = [2]                                              from Bubble up shape for Sym(97) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   V27 = [0  0]                                           from Bubble up value for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : POST:   S27 = [2]                                              from Bubble up shape for Sym(27) <- With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : PRE:    length(S27) = length([u2655])                          from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : PRE:    S97 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : PRE:    S29 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : PRE:    S27 = S27                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // RuntimeCheck : PRE:    V27 < V97                                              from With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      // Shape: V101=[u695] and S101=[]
                      // with: With(lb=Sym(27) lbStrict=Sym(31) ubStict=Sym(31) ub=Sym(97) step=Sym(29) width=Sym(27)  Sym(98) => Sym(100))
                      val lb0: Int = x27.content()(0)
                      val ub0: Int = x97.content()(0)
                      val step0: Int = x29.content()(0)
                      val width0: Int = x27.content()(0)
                      val ll0: Int = if (x31) lb0 + 1 else lb0
                      val ul0: Int = if (x31) ub0 else ub0 + 1
                      for (iv0 <- List.range(ll0, ul0)) {
                        if ((iv0 - lb0) % step0 <= width0) {
                          val lb1: Int = x27.content()(1)
                          val ub1: Int = x97.content()(1)
                          val step1: Int = x29.content()(1)
                          val width1: Int = x27.content()(1)
                          val ll1: Int = if (x31) lb1 + 1 else lb1
                          val ul1: Int = if (x31) ub1 else ub1 + 1
                          for (iv1 <- List.range(ll1, ul1)) {
                            if ((iv1 - lb1) % step1 <= width1) {
                              val x98: MDArray[Int] = iv0::iv1::Nil
                              val iv: MDArray[Int] = x98
                              val feval: MDArray[Int] = {
                                // RuntimeCheck : POST:   V96 = [u681  u680]                                     from Bubble up value for Sym(96) <- InfixOp(+: Sym(98) and Sym(96))
                                // RuntimeCheck : POST:   S96 = [2]                                              from Bubble up shape for Sym(96) <- InfixOp(+: Sym(98) and Sym(96))
                                // RuntimeCheck : POST:   S98 = [2]                                              from Bubble up shape for Sym(98) <- InfixOp(+: Sym(98) and Sym(96))
                                // RuntimeCheck : PRE:    S98 = S96 OR S96 = []                                  from InfixOp(+: Sym(98) and Sym(96))
                                // Shape: V99=[u754(<10)  u755(<10)] and S99=[2]
                                val x99: MDArray[Int] = {
                                  val result = new Array[Int](shape(x98).content().foldLeft(1)((a,b) => a*b))
                                  for(i <- List.range(0, result.length))
                                  result(i) = x98.content()(i) +  x96.content()(i)
                                  internalReshape(shape(x98), result, "infixOpAA")
                                }
                                // RuntimeCheck : POST:   V69 = [u579  u578  u577  u576  u575  u574  u573  u572  u571  u570  u569  u568  u567  u566  u565  u564  u563  u562  u561  u560  u559  u558  u557  u556  u555  u554  u553  u552  u551  u550  u549  u548  u547  u546  u545  u544  u543  u542  u541  u540  u539  u538  u537  u536  u535  u534  u533  u532  u531  u530  u529  u528  u527  u526  u525  u524  u523  u522  u521  u520  u519  u518  u517  u516  u515  u514  u513  u512  u511  u510  u509  u508  u507  u506  u505  u504  u503  u502  u501  u500  u499  u498  u497  u496  u495  u494  u493  u492  u491  u490  u489  u488  u487  u486  u485  u484  u483  u482  u481  u480]     from Bubble up value for Sym(69) <- Sel(Sym(99), Sym(69))
                                // RuntimeCheck : POST:   S69 = [10  10]                                         from Bubble up shape for Sym(69) <- Sel(Sym(99), Sym(69))
                                // RuntimeCheck : POST:   V99 = [u754(<10)  u755(<10)]                           from Bubble up value for Sym(99) <- Sel(Sym(99), Sym(69))
                                // RuntimeCheck : POST:   S99 = [2]                                              from Bubble up shape for Sym(99) <- Sel(Sym(99), Sym(69))
                                // RuntimeCheck : PRE:    length(S99) = length([u2672])                          from Sel(Sym(99), Sym(69))
                                // RuntimeCheck : PRE:    S69(:length(V99)) < V99                                from Sel(Sym(99), Sym(69))
                                // Shape: V100=[u696] and S100=[]
                                
                                // Shape: V100=[u696] and S100=[]
                                val x100: Int = x69.content()(flatten(shape(x69), x99, "sel"))
                                x100
                              }
                              // the action of this loop:
                              if (result == null) {
                                // create the array and shape
                                result = new Array[Int](x21.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                rshape = shape(feval).content()
                              } else {
                                // check shape -- this WILL be redundant due to runtime checks
                                if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                              }
                              // copy new content
                              val mainIndex: Int = flatten(x21 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                              for (innerIndex <- List.range(0, rshape.length)) {
                                result(mainIndex + innerIndex) = feval(innerIndex)
                              }
                            } // if ((iv0 ...
                          } // for (iv0 ...
                        } // if ((iv1 ...
                      } // for (iv1 ...
                      internalReshape(x21 ::: rshape.toList, result, opName)
                    }
                    
                    // RuntimeCheck : POST:   V102 = [u721  u720  u719  u718  u717  u716  u715  u714  u713]     from Bubble up value for Sym(102) <- Dim(Sym(102))
                    // RuntimeCheck : POST:   S102 = [3  3]                                          from Bubble up shape for Sym(102) <- Dim(Sym(102))
                    // Shape: V103=[2] and S103=[]
                    val x103: Int = dim(x102)
                    // RuntimeCheck : POST:   V103 = [2]                                             from Bubble up value for Sym(103) <- FromValue(Sym(103))
                    // RuntimeCheck : POST:   S103 = []                                              from Bubble up shape for Sym(103) <- FromValue(Sym(103))
                    // Shape: V104=[2] and S104=[]
                    val x104: Int = x103
                    // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(104))
                    // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(104))
                    // RuntimeCheck : POST:   V104 = [2]                                             from Bubble up value for Sym(104) <- Values(Sym(10), Sym(104))
                    // RuntimeCheck : POST:   S104 = []                                              from Bubble up shape for Sym(104) <- Values(Sym(10), Sym(104))
                    // RuntimeCheck : PRE:    S104 = []                                              from Values(Sym(10), Sym(104))
                    // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(104))
                    // Shape: V105=[0  0] and S105=[2]
                    val x105: MDArray[Int] = {
                      val result = new Array[Int](x104)
                      for(i <- List.range(0, result.length))
                      result(i) = x10
                      internalReshape(x104::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V102 = [u721  u720  u719  u718  u717  u716  u715  u714  u713]     from Bubble up value for Sym(102) <- Shape(Sym(102))
                    // RuntimeCheck : POST:   S102 = [3  3]                                          from Bubble up shape for Sym(102) <- Shape(Sym(102))
                    // Shape: V106=[3  3] and S106=[2]
                    val x106: MDArray[Int] = shape(x102)
                    // RuntimeCheck : POST:   V105 = [0  0]                                          from Bubble up value for Sym(105) <- Shape(Sym(105))
                    // RuntimeCheck : POST:   S105 = [2]                                             from Bubble up shape for Sym(105) <- Shape(Sym(105))
                    // Shape: V107=[2] and S107=[1]
                    val x107: MDArray[Int] = shape(x105)
                    // RuntimeCheck : POST:   V107 = [2]                                             from Bubble up value for Sym(107) <- Sel(Sym(7), Sym(107))
                    // RuntimeCheck : POST:   S107 = [1]                                             from Bubble up shape for Sym(107) <- Sel(Sym(7), Sym(107))
                    // RuntimeCheck : POST:   V7 = [0]                                               from Bubble up value for Sym(7) <- Sel(Sym(7), Sym(107))
                    // RuntimeCheck : POST:   S7 = [1]                                               from Bubble up shape for Sym(7) <- Sel(Sym(7), Sym(107))
                    // RuntimeCheck : PRE:    length(S7) = length([u2359])                           from Sel(Sym(7), Sym(107))
                    // RuntimeCheck : PRE:    S107(:length(V7)) < V7                                 from Sel(Sym(7), Sym(107))
                    // Shape: V108=[2] and S108=[]
                    
                    // Shape: V108=[2] and S108=[]
                    val x108: Int = x107.content()(flatten(shape(x107), x7, "sel"))
                    // RuntimeCheck : POST:   V12 = [1]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(108))
                    // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(108))
                    // RuntimeCheck : POST:   V108 = [2]                                             from Bubble up value for Sym(108) <- Values(Sym(12), Sym(108))
                    // RuntimeCheck : POST:   S108 = []                                              from Bubble up shape for Sym(108) <- Values(Sym(12), Sym(108))
                    // RuntimeCheck : PRE:    S108 = []                                              from Values(Sym(12), Sym(108))
                    // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(108))
                    // Shape: V109=[1  1] and S109=[2]
                    val x109: MDArray[Int] = {
                      val result = new Array[Int](x108)
                      for(i <- List.range(0, result.length))
                      result(i) = x12
                      internalReshape(x108::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V10 = [0]                                              from Bubble up value for Sym(10) <- Values(Sym(10), Sym(108))
                    // RuntimeCheck : POST:   S10 = []                                               from Bubble up shape for Sym(10) <- Values(Sym(10), Sym(108))
                    // RuntimeCheck : POST:   V108 = [2]                                             from Bubble up value for Sym(108) <- Values(Sym(10), Sym(108))
                    // RuntimeCheck : POST:   S108 = []                                              from Bubble up shape for Sym(108) <- Values(Sym(10), Sym(108))
                    // RuntimeCheck : PRE:    S108 = []                                              from Values(Sym(10), Sym(108))
                    // RuntimeCheck : PRE:    S10 = []                                               from Values(Sym(10), Sym(108))
                    // Shape: V110=[0  0] and S110=[2]
                    val x110: MDArray[Int] = {
                      val result = new Array[Int](x108)
                      for(i <- List.range(0, result.length))
                      result(i) = x10
                      internalReshape(x108::Nil, result, "values")
                    }
                    // RuntimeCheck : POST:   V115 = [u691]                                          from Bubble up value for Sym(115) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S115 = []                                              from Bubble up shape for Sym(115) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   V114 = [u752(<3)  u753(<3)]                            from Bubble up value for Sym(114) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S114 = [2]                                             from Bubble up shape for Sym(114) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   V110 = [0  0]                                          from Bubble up value for Sym(110) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S110 = [2]                                             from Bubble up shape for Sym(110) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   V109 = [1  1]                                          from Bubble up value for Sym(109) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S109 = [2]                                             from Bubble up shape for Sym(109) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   V16 = [u116]                                           from Bubble up value for Sym(16) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S16 = []                                               from Bubble up shape for Sym(16) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   V106 = [3  3]                                          from Bubble up value for Sym(106) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S106 = [2]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   V31 = [u87]                                            from Bubble up value for Sym(31) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S31 = []                                               from Bubble up shape for Sym(31) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   V105 = [0  0]                                          from Bubble up value for Sym(105) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : POST:   S105 = [2]                                             from Bubble up shape for Sym(105) <- With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : PRE:    length(S105) = length([u1478])                         from With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : PRE:    S31 = []                                               from With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : PRE:    S16 = []                                               from With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : PRE:    S106 = S105                                            from With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : PRE:    S109 = S105                                            from With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : PRE:    S110 = S105                                            from With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // RuntimeCheck : PRE:    V105 < V106                                            from With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    // Shape: V116=[u690] and S116=[]
                    // with: With(lb=Sym(105) lbStrict=Sym(31) ubStict=Sym(16) ub=Sym(106) step=Sym(109) width=Sym(110)  Sym(114) => Sym(115))
                    val lb0: Int = x105.content()(0)
                    val ub0: Int = x106.content()(0)
                    val step0: Int = x109.content()(0)
                    val width0: Int = x110.content()(0)
                    val ll0: Int = if (x31) lb0 + 1 else lb0
                    val ul0: Int = if (x16) ub0 else ub0 + 1
                    for (iv0 <- List.range(ll0, ul0)) {
                      if ((iv0 - lb0) % step0 <= width0) {
                        val lb1: Int = x105.content()(1)
                        val ub1: Int = x106.content()(1)
                        val step1: Int = x109.content()(1)
                        val width1: Int = x110.content()(1)
                        val ll1: Int = if (x31) lb1 + 1 else lb1
                        val ul1: Int = if (x16) ub1 else ub1 + 1
                        for (iv1 <- List.range(ll1, ul1)) {
                          if ((iv1 - lb1) % step1 <= width1) {
                            val x114: MDArray[Int] = iv0::iv1::Nil
                            val iv: MDArray[Int] = x114
                            val feval: MDArray[Int] = {
                              // RuntimeCheck : POST:   V102 = [u721  u720  u719  u718  u717  u716  u715  u714  u713]     from Bubble up value for Sym(102) <- Sel(Sym(114), Sym(102))
                              // RuntimeCheck : POST:   S102 = [3  3]                                          from Bubble up shape for Sym(102) <- Sel(Sym(114), Sym(102))
                              // RuntimeCheck : POST:   V114 = [u752(<3)  u753(<3)]                            from Bubble up value for Sym(114) <- Sel(Sym(114), Sym(102))
                              // RuntimeCheck : POST:   S114 = [2]                                             from Bubble up shape for Sym(114) <- Sel(Sym(114), Sym(102))
                              // RuntimeCheck : PRE:    length(S114) = length([u2653])                         from Sel(Sym(114), Sym(102))
                              // RuntimeCheck : PRE:    S102(:length(V114)) < V114                             from Sel(Sym(114), Sym(102))
                              // Shape: V115=[u691] and S115=[]
                              
                              // Shape: V115=[u691] and S115=[]
                              val x115: Int = x102.content()(flatten(shape(x102), x114, "sel"))
                              x115
                            }
                            // the action of this loop:
                            result = foldFunction(result, feval)
                          } // if ((iv0 ...
                        } // for (iv0 ...
                      } // if ((iv1 ...
                    } // for (iv1 ...
                    result
                  }
                  
                  // RuntimeCheck : POST:   V117 = [u127]                                          from Bubble up value for Sym(117) <- ToValue(Sym(117))
                  // RuntimeCheck : POST:   S117 = []                                              from Bubble up shape for Sym(117) <- ToValue(Sym(117))
                  // RuntimeCheck : PRE:    length(S117) = length([])                              from ToValue(Sym(117))
                  // Shape: V118=[u62] and S118=[]
                  val x118: Int = x117
                  // RuntimeCheck : POST:   V118 = [u62]                                           from Bubble up value for Sym(118) <- FromValue(Sym(118))
                  // RuntimeCheck : POST:   S118 = []                                              from Bubble up shape for Sym(118) <- FromValue(Sym(118))
                  // Shape: V119=[u61] and S119=[]
                  val x119: Int = x118
                  // RuntimeCheck : POST:   V119 = [u61]                                           from Bubble up value for Sym(119) <- InfixOp(-: Sym(95) and Sym(119))
                  // RuntimeCheck : POST:   S119 = []                                              from Bubble up shape for Sym(119) <- InfixOp(-: Sym(95) and Sym(119))
                  // RuntimeCheck : POST:   V95 = [u93]                                            from Bubble up value for Sym(95) <- InfixOp(-: Sym(95) and Sym(119))
                  // RuntimeCheck : POST:   S95 = []                                               from Bubble up shape for Sym(95) <- InfixOp(-: Sym(95) and Sym(119))
                  // RuntimeCheck : PRE:    S95 = S119 OR S119 = []                                from InfixOp(-: Sym(95) and Sym(119))
                  // Shape: V120=[u92] and S120=[]
                  val x120: Int = {
                    val result = new Array[Int](shape(x95).content().foldLeft(1)((a,b) => a*b))
                    for(i <- List.range(0, result.length))
                    result(i) = x95.content()(i) -  x119
                    internalReshape(shape(x95), result, "infixOpAA")
                  }
                  // RuntimeCheck : POST:   V20 = [3]                                              from Bubble up value for Sym(20) <- InfixOp(===: Sym(120) and Sym(20))
                  // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(===: Sym(120) and Sym(20))
                  // RuntimeCheck : POST:   V120 = [u92]                                           from Bubble up value for Sym(120) <- InfixOp(===: Sym(120) and Sym(20))
                  // RuntimeCheck : POST:   S120 = []                                              from Bubble up shape for Sym(120) <- InfixOp(===: Sym(120) and Sym(20))
                  // RuntimeCheck : PRE:    S120 = S20 OR S20 = []                                 from InfixOp(===: Sym(120) and Sym(20))
                  // Shape: V124=[u91] and S124=[]
                  val x124: Boolean = {
                    val result = new Array[Boolean](shape(x120).content().foldLeft(1)((a,b) => a*b))
                    for(i <- List.range(0, result.length))
                    result(i) = x120.content()(i) ===  x20
                    internalReshape(shape(x120), result, "infixOpAA")
                  }
                  // RuntimeCheck : POST:   V124 = [u91]                                           from Bubble up value for Sym(124) <- ToValue(Sym(124))
                  // RuntimeCheck : POST:   S124 = []                                              from Bubble up shape for Sym(124) <- ToValue(Sym(124))
                  // RuntimeCheck : PRE:    length(S124) = length([])                              from ToValue(Sym(124))
                  // Shape: V125=[u58] and S125=[]
                  val x125: Boolean = x124
                  val x126 = if (x125) {
                    x12
                  } else {
                    x10
                  }
                  x126
                } else {
                  x10
                }
                x127
              }
              // the action of this loop:
              if (result == null) {
                // create the array and shape
                result = new Array[Int](x6.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                rshape = shape(feval).content()
              } else {
                // check shape -- this WILL be redundant due to runtime checks
                if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
              }
              // copy new content
              val mainIndex: Int = flatten(x6 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
              for (innerIndex <- List.range(0, rshape.length)) {
                result(mainIndex + innerIndex) = feval(innerIndex)
              }
            } // if ((iv0 ...
          } // for (iv0 ...
        } // if ((iv1 ...
      } // for (iv1 ...
      internalReshape(x6 ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   V69 = [u579  u578  u577  u576  u575  u574  u573  u572  u571  u570  u569  u568  u567  u566  u565  u564  u563  u562  u561  u560  u559  u558  u557  u556  u555  u554  u553  u552  u551  u550  u549  u548  u547  u546  u545  u544  u543  u542  u541  u540  u539  u538  u537  u536  u535  u534  u533  u532  u531  u530  u529  u528  u527  u526  u525  u524  u523  u522  u521  u520  u519  u518  u517  u516  u515  u514  u513  u512  u511  u510  u509  u508  u507  u506  u505  u504  u503  u502  u501  u500  u499  u498  u497  u496  u495  u494  u493  u492  u491  u490  u489  u488  u487  u486  u485  u484  u483  u482  u481  u480]     from Bubble up value for Sym(69) <- InfixOp(-: Sym(3) and Sym(69))
    // RuntimeCheck : POST:   S69 = [10  10]                                         from Bubble up shape for Sym(69) <- InfixOp(-: Sym(3) and Sym(69))
    // RuntimeCheck : POST:   V3 = [u679  u678  u677  u676  u675  u674  u673  u672  u671  u670  u669  u668  u667  u666  u665  u664  u663  u662  u661  u660  u659  u658  u657  u656  u655  u654  u653  u652  u651  u650  u649  u648  u647  u646  u645  u644  u643  u642  u641  u640  u639  u638  u637  u636  u635  u634  u633  u632  u631  u630  u629  u628  u627  u626  u625  u624  u623  u622  u621  u620  u619  u618  u617  u616  u615  u614  u613  u612  u611  u610  u609  u608  u607  u606  u605  u604  u603  u602  u601  u600  u599  u598  u597  u596  u595  u594  u593  u592  u591  u590  u589  u588  u587  u586  u585  u584  u583  u582  u581  u580]     from Bubble up value for Sym(3) <- InfixOp(-: Sym(3) and Sym(69))
    // RuntimeCheck : POST:   S3 = [10  10]                                          from Bubble up shape for Sym(3) <- InfixOp(-: Sym(3) and Sym(69))
    // RuntimeCheck : PRE:    S3 = S69 OR S69 = []                                   from InfixOp(-: Sym(3) and Sym(69))
    // Shape: V130=[u379  u378  u377  ...  u282  u281  u280] and S130=[10  10]
    val x130: MDArray[Int] = {
      val result = new Array[Int](shape(x3).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = x3.content()(i) -  x69.content()(i)
      internalReshape(shape(x3), result, "infixOpAA")
    }
    // RuntimeCheck : POST:   V129 = [u479  u478  u477  u476  u475  u474  u473  u472  u471  u470  u469  u468  u467  u466  u465  u464  u463  u462  u461  u460  u459  u458  u457  u456  u455  u454  u453  u452  u451  u450  u449  u448  u447  u446  u445  u444  u443  u442  u441  u440  u439  u438  u437  u436  u435  u434  u433  u432  u431  u430  u429  u428  u427  u426  u425  u424  u423  u422  u421  u420  u419  u418  u417  u416  u415  u414  u413  u412  u411  u410  u409  u408  u407  u406  u405  u404  u403  u402  u401  u400  u399  u398  u397  u396  u395  u394  u393  u392  u391  u390  u389  u388  u387  u386  u385  u384  u383  u382  u381  u380]     from Bubble up value for Sym(129) <- InfixOp(+: Sym(130) and Sym(129))
    // RuntimeCheck : POST:   S129 = [10  10]                                        from Bubble up shape for Sym(129) <- InfixOp(+: Sym(130) and Sym(129))
    // RuntimeCheck : POST:   V130 = [u379  u378  u377  u376  u375  u374  u373  u372  u371  u370  u369  u368  u367  u366  u365  u364  u363  u362  u361  u360  u359  u358  u357  u356  u355  u354  u353  u352  u351  u350  u349  u348  u347  u346  u345  u344  u343  u342  u341  u340  u339  u338  u337  u336  u335  u334  u333  u332  u331  u330  u329  u328  u327  u326  u325  u324  u323  u322  u321  u320  u319  u318  u317  u316  u315  u314  u313  u312  u311  u310  u309  u308  u307  u306  u305  u304  u303  u302  u301  u300  u299  u298  u297  u296  u295  u294  u293  u292  u291  u290  u289  u288  u287  u286  u285  u284  u283  u282  u281  u280]     from Bubble up value for Sym(130) <- InfixOp(+: Sym(130) and Sym(129))
    // RuntimeCheck : POST:   S130 = [10  10]                                        from Bubble up shape for Sym(130) <- InfixOp(+: Sym(130) and Sym(129))
    // RuntimeCheck : PRE:    S130 = S129 OR S129 = []                               from InfixOp(+: Sym(130) and Sym(129))
    // Shape: V131=[u279  u278  u277  ...  u182  u181  u180] and S131=[10  10]
    val x131: MDArray[Int] = {
      val result = new Array[Int](shape(x130).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = x130.content()(i) +  x129.content()(i)
      internalReshape(shape(x130), result, "infixOpAA")
    }
    x131
  }
}
/*****************************************
  End of Generated Code                  
*******************************************/