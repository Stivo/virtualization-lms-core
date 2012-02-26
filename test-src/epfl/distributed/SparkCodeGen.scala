package scala.virtualization.lms
package epfl
package distributed

import scala.virtualization.lms.common.ScalaGenBase
import java.io.PrintWriter

trait SparkVectorOpsExp extends VectorOpsExp {
   case class VectorReduceByKey[K: Manifest, V : Manifest](in : Exp[Vector[(K,V)]], func : (Exp[V], Exp[V]) => Exp[V]) 
    	extends Def[Vector[(K, V)]]{ 
    //with TypedNode [(K, Iterable[V]),(K,V)]{
      val mKey = manifest[K]
      val mValue = manifest[V]
      lazy val closure = doLambda2(func)(getClosureTypes._2, getClosureTypes._2, getClosureTypes._2, FakeSourceContext())
      def getClosureTypes = (manifest[(V,V)], manifest[V])
      
//      def getTypes = (mKey, mValue)
    }

    override def syms(e: Any): List[Sym[Any]] = e match { 
    case s: ClosureNode[_,_]  => syms(s.in, s.closure) ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case s: VectorReduce[_,_]  => syms(s.in, s.closure) ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case _ => super.syms(e)
  }
    
   override def readSyms(e: Any): List[Sym[Any]] = e match { //TR FIXME: check this is actually correct
    case s: ClosureNode[_,_]  => syms(s.closure, s.in) ++ super.syms(e)// super call: add case class syms (iff flag is set)
    case _ => super.readSyms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case s: ClosureNode[_,_]  => effectSyms(s.closure, s.in) 
    case _ => super.boundSyms(e)
  }

  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: ClosureNode[_,_]  => freqHot(s.closure) ++ freqNormal(s.in) 
    case s: VectorReduce[_,_]  => freqHot(s.closure) ++ freqNormal(s.in) 
    case _ => super.symsFreq(e)
  }
}


trait SparkVectorOpsExpOpt extends SparkVectorOpsExp {
//  val IR : VectorOpsExp
//  import IR.{VectorMap}
//  def compose[A, B, C](f1 : A => B, f2 : B=>C) : A => C = {x : A => f2(f1(x)) }

  override def vector_reduce[K: Manifest, V : Manifest](vector : Exp[Vector[(K,Iterable[V])]], f : (Exp[V], Exp[V]) => Exp[V] ) = vector match {
    case Def(VectorGroupByKey(in)) => VectorReduceByKey(in, f)
    case _ => super.vector_reduce(vector, f)
  }

   override def syms(e: Any): List[Sym[Any]] = e match {
     case red : VectorReduceByKey[_,_] => syms(red.in, red.closure)
    case _ => super.syms(e)
  }
      
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
     case red : VectorReduceByKey[_,_] => freqHot(red.closure) ++ freqNormal(red.in)
    case _ => super.symsFreq(e)
  }
  
}


trait SparkGenVector extends ScalaGenBase {
  val IR: SparkVectorOpsExp
	import IR.{Sym, Def, Exp, Reify, Reflect, Const}
	import IR.{NewVector, VectorSave, VectorMap, VectorFilter, VectorFlatMap, VectorFlatten, VectorGroupByKey, VectorReduce}
	import IR.{VectorReduceByKey}
	import IR.{findDefinition, fresh, reifyEffects, reifyEffectsHere,toAtom}
//	
//	def emitFunction[A,B](f: Exp[A] => Exp[B], stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]) : String = {
//	  val sc = SourceContext("test",Nil)
//	  val y = fresh[A]
////	  val applied = IR.doApply(IR.doLambda(f), y).asInstanceOf[Sym[B]]
//	  val closure = IR.doLambda(f)(mA, mB, sc).asInstanceOf[Sym[A=>B]]
////	  println(closure.Type)
////	  val def_ = IR.findDefinition(applied)//(stream)
////	  emitNode(applied, def_.get.rhs)(stream)
//	  val def_ = IR.findDefinition(closure)//(stream)
//	  emitNode(closure, def_.get.rhs)(stream)
//	  println(def_.get.rhs)
//	  quote(closure)
////	  stream.println(closure)
////	  quote(applied)
//	}
	
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = 
    {val out = rhs match {
      case nv@NewVector(filename) => emitValDef(sym, "sc.textFile(%s)".format(quote(filename)))
      case vs@VectorSave(vector, filename) => stream.println("%s.saveAsTextFile(%s)".format(quote(vector), quote(filename)))
      case vm@VectorMap(vector, function) => emitValDef(sym, "%s.map(%s)".format(quote(vector), quote(vm.closure)))
      case vf@VectorFilter(vector, function) => emitValDef(sym, "%s.filter(%s)".format(quote(vector), quote(vf.closure)))
//      case vm@VectorFlatMap(vector, function) => emitValDef(sym, "flat mapping vector %s with function %s".format(vector, function))
//      case vm@VectorFlatten(v1, v2) => emitValDef(sym, "flattening vector %s with vector %s".format(v1, v2))
      case gbk@VectorGroupByKey(vector) => emitValDef(sym, "%s.groupByKey".format(quote(vector)))
      case red@VectorReduce(vector, f) => emitValDef(sym, "%s.map(x => (x._1,x._2.reduce(%s)))".format(quote(vector), quote(red.closure)))
      case red@VectorReduceByKey(vector, f) => emitValDef(sym, "%s.reduceByKey(%s)".format(quote(vector), quote(red.closure)))
    case _ => super.emitNode(sym, rhs)
  }
    println(sym+" "+rhs)
    out
    }
    
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val func : Exp[A] => Exp[B] = {x => reifyEffects(f(x))}
    
    val x = fresh[A]
    val y = func(x)

    val sA = mA.toString
    val sB = mB.toString

    val staticData = getFreeDataBlock(y)

    stream.println("/*****************************************\n"+
                   "  Emitting Spark Code                  \n"+
                   "*******************************************/")
    stream.println("""
import scala.math.random
import spark._
import SparkContext._

object %s {
        def main(args: Array[String]) {
    		val sc = new SparkContext(args(0), "%s")
        """.format(className, className))
    	
    // TODO: separate concerns, should not hard code "pxX" name scheme for static data here
    
    emitBlock(y)(stream)
    
    
    stream.println("}")
    
    stream.println("}")
    stream.println("/*****************************************\n"+
                   "  End of Spark Code                  \n"+
                   "*******************************************/")

    stream.flush
    
    staticData
//    Nil
  }

    
}

trait SparkGen extends VectorBaseCodeGenPkg with SparkGenVector
