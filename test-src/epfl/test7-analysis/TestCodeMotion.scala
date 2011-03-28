package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



trait NestLambdaProg extends Arith with Functions with Print { // also used by TestLambdaLift
  
  def test(x: Rep[Unit]) = {
    val f = doLambda { x: Rep[Double] =>
      val g = doLambda { y: Rep[Double] =>
        print("yo")
        y + (unit(4.0) * unit(3.0))
      }
      g
    }
    f
  }
  
}

trait NestCondProg extends Arith with Functions with IfThenElse with Print {
  
  /* TODO: This program exhibits behavior that is likely undesired in many
  cases. The definition of f will be moved *into* g and into the conditional.
  The doLambda in the else branch will not be hoisted out of g either.
  
  While there are situations where this particular kind of code motion
  is an improvement (namely, if the probability of y == true is very low
  and the else branch would be cheap).
  */
  
  
  def test(x: Rep[Unit]) = {
    val f = doLambda { x: Rep[Double] => 2 * x }
    
    val g = doLambda { y: Rep[Boolean] =>
      print("yo")
      if (y)
        f
      else
        doLambda { x: Rep[Double] => x + 1 }
   }
    g
  }
  
}




class TestCodemotion extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test7-"
  
  def testCodemotion1 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion1") {
      new NestLambdaProg with ArithExp with FunctionsExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion1")
  }

  def testCodemotion2 = {
    // test loop hoisting (should use loops but lambdas will do for now)
    withOutFile(prefix+"codemotion2") {
      new NestCondProg with ArithExp with FunctionsExp with IfThenElseExp with PrintExp { self =>
        val codegen = new ScalaGenArith with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"codemotion2")
  }


}