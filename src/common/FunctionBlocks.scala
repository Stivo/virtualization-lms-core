package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.{Blocks, Effects, AbstractTransformer}
import scala.virtualization.lms.util.OverloadHack

/*
 * This trait allows functions to be stored as case class parameters in a way that works correctly
 * with transformers. Pre-transformation, they are converted from lambdas to BlockN classes,
 * and then post transformation they are converted from BlockN classes back to lambdas (with the
 * correctly transformed free and bound vars).
 */
trait FunctionBlocksExp extends BaseExp with Blocks with Effects with OverloadHack {
  /*
   * BlockN definitions
   */
  implicit def lambdaToBlock0[R:Manifest](f: () => Exp[R]) = Block0(reifyEffects(f()))
  case class Block0[R:Manifest](blockRes: Block[R])
     
  implicit def lambdaToBlock1[T1:Manifest,R:Manifest](f: Exp[T1] => Exp[R]) = {
    val x1 = fresh[T1]
    Block1(x1,reifyEffects(f(x1)))
  }    
  case class Block1[T1:Manifest,R:Manifest](blockArg1: Sym[T1], blockRes: Block[R]) 
  
  implicit def lambdaToBlock2[T1:Manifest,T2:Manifest,R:Manifest](f: (Exp[T1],Exp[T2]) => Exp[R]) = {
    val (x1,x2) = (fresh[T1],fresh[T2])
    Block2(x1,x2,reifyEffects(f(x1,x2)))
  }
  case class Block2[T1:Manifest,T2:Manifest,R:Manifest](blockArg1: Sym[T1], blockArg2: Sym[T2], blockRes: Block[R]) 
 
 
  /*
   * boundSyms override required for all BlockNs
   */
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Block0(blk) => effectSyms(blk)
    case Block1(b1,blk) => scala.List(b1) ::: effectSyms(blk)
    case Block2(b1,b2,blk) => scala.List(b1,b2) ::: effectSyms(blk)
    case _ => super.boundSyms(e)
  }
 
  
  /*
   * Enable transformation of BlockN types back to lambdas
   */
  implicit def transformerToBlockTransformer(t: ForwardTransformer{val IR: FunctionBlocksExp.this.type}) = new {
    def apply[R](x: Block0[R]): (() => Exp[R]) =  { () => t.reflectBlock(x.blockRes) }
    def apply[T1,R](x: Block1[T1,R]): Exp[T1] => Exp[R] = { a => t.subst += (x.blockArg1 -> a); t.reflectBlock(x.blockRes) }
    def apply[T1,T2,R](x: Block2[T1,T2,R]): (Exp[T1],Exp[T2]) => Exp[R] =  { (a,b) => t.subst ++= scala.List(x.blockArg1 -> a, x.blockArg2 -> b); t.reflectBlock(x.blockRes) }    
  }  
}
  
