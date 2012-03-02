package scala.virtualization.lms
package epfl
package distributed

import scala.virtualization.lms.common.ScalaGenBase
import java.io.PrintWriter

trait SparkProgram extends VectorOpsExp with VectorImplOps with SparkVectorOpsExp with SparkVectorOpsExpOpt {
  
}

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
    case s: ClosureNode[_,_] if s.declareClosureAsSym => syms(s.in, s.closure)// ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case s: ClosureNode[_,_] => syms(s.in)// ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case s: VectorReduce[_,_]  => syms(s.in, s.closure) ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case _ => super.syms(e)
  }
    
//   override def readSyms(e: Any): List[Sym[Any]] = e match { //TR FIXME: check this is actually correct
//    case s: ClosureNode[_,_]  => syms(s.closure, s.in) ++ super.syms(e)// super call: add case class syms (iff flag is set)
//    case _ => super.readSyms(e)
//  }
//  
//  override def boundSyms(e: Any): List[Sym[Any]] = e match {
//    case s: ClosureNode[_,_]  => effectSyms(s.closure, s.in) 
//    case _ => super.boundSyms(e)
//  }

  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: ClosureNode[_,_] if s.declareClosureAsSym => freqNormal(s.in)++freqHot(s.closure) 
    case s: ClosureNode[_,_] => freqNormal(s.in) 
//    case s: ClosureNode[_,_]  => freqHot(s.closure) ++ freqNormal(s.in) 
    case s: VectorReduce[_,_]  => freqHot(s.closure) ++ freqNormal(s.in) 
    case _ => super.symsFreq(e)
  }
}


trait SparkVectorOpsExpOpt extends SparkVectorOpsExp {
//  val IR : VectorOpsExp
//  import IR.{VectorMap}
//  def compose[A, B, C](f1 : A => B, f2 : B=>C) : A => C = {x : A => f2(f1(x)) }

//  override def vector_reduce[K: Manifest, V : Manifest](vector : Exp[Vector[(K,Iterable[V])]], f : (Exp[V], Exp[V]) => Exp[V] ) = vector match {
//    case Def(VectorGroupByKey(in)) => VectorReduceByKey(in, f)
//    case _ => super.vector_reduce(vector, f)
//  }

//  override def vector_map[A : Manifest, B : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[B]) = vector match {
//	case Def(vm@VectorMap(in, f2)) => VectorMap(in, f2.andThen(f))(mtype(manifest[vm.mA]),manifest[B])
//	case _ => super.vector_map(vector, f)
//  }
  
   override def syms(e: Any): List[Sym[Any]] = e match {
     case red : VectorReduceByKey[_,_] => syms(red.in, red.closure)
    case _ => super.syms(e)
  }
      
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
     case red : VectorReduceByKey[_,_] => freqHot(red.closure) ++ freqNormal(red.in)
    case _ => super.symsFreq(e)
  }
  
}

trait SparkTransformations extends VectorTransformations {
    val IR: VectorOpsExp with SparkVectorOpsExp
  	import IR.{VectorReduceByKey, VectorReduce, VectorGroupByKey, VectorMap}
    import IR.{Def, Exp}

  	class ReduceByKeyTransformation extends SimpleSingleConsumerTransformation {
  	  
	   def doTransformationPure(inExp : Exp[_]) = inExp match {
            case Def(red@VectorReduce(Def(gbk@VectorGroupByKey(v1)),f1)) => {
              new VectorReduceByKey(v1, f1)
            }
            case _ => null
	   }
	}

  	class MapMergeTransformation extends SimpleSingleConsumerTransformation {
  	  
	   def doTransformationPure(inExp : Exp[_]) = inExp match {
            case Def(red@VectorMap(Def(gbk@VectorMap(v1, f2)),f1)) => {
              VectorMap(v1, f2.andThen(f1))(gbk.mA,red.mB)
            }
            case _ => null
	   }
	}
  	
  	class MakeClosuresTransformation extends Transformation {

 	   def appliesToNode(inExp : Exp[_], t : Transformer) = {
	     inExp match {
	       case Def(s : IR.ClosureNode[_,_]) => true
	       case s : IR.Sym[_] if IR.findDefinition(s).isDefined => true
	       case Def(VectorReduceByKey(in, func)) => true
	       case _ => false
	     }
	   }
 	   def doTransformation(inExp : Exp[_]) : Def[_] = inExp match {
 	     case Def(s : IR.ClosureNode[_,_]) => s.declareClosureAsSym = true; s
 	     case s : IR.Sym[_] if IR.findDefinition(s).isDefined => IR.findDefinition(s).get.rhs
	     case Def(r@VectorReduceByKey(in, func)) => r
 	     case _ => null
 	   }
	}

}

trait SparkGenVector extends ScalaGenBase with ScalaGenVector with SparkTransformations {
  
//    val IR: VectorOpsExp
	import IR.{Sym, Def, Exp, Reify, Reflect, Const}
	import IR.{NewVector, VectorSave, VectorMap, VectorFilter, VectorFlatMap, VectorFlatten, VectorGroupByKey, VectorReduce
	  , ComputationNode, VectorSaves}
	import IR.{TTP, TP, SubstTransformer, IRNode}
	import IR.{findDefinition}
	import IR.{ClosureNode, freqHot, freqNormal, Lambda}

  val IR: SparkVectorOpsExp
	import IR.{GetArgs}
	import IR.{VectorReduceByKey}
	import IR.{findDefinition, fresh, reifyEffects, reifyEffectsHere,toAtom}
		
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = 
    {val out = rhs match {
      case nv@NewVector(filename) => emitValDef(sym, "sc.textFile(%s)".format(quote(filename)))
      case vs@VectorSave(vector, filename) => stream.println("%s.saveAsTextFile(%s)".format(quote(vector), quote(filename)))
      case vm@VectorMap(vector, function) => emitValDef(sym, "%s.map(%s)".format(quote(vector), quote(vm.closure)))
      case vf@VectorFilter(vector, function) => emitValDef(sym, "%s.filter(%s)".format(quote(vector), quote(vf.closure)))
      case vm@VectorFlatMap(vector, function) => emitValDef(sym, "%s.flatMap(%s)".format(quote(vector), quote(vm.closure)))
//      case vm@VectorFlatten(v1, v2) => emitValDef(sym, "flattening vector %s with vector %s".format(v1, v2))
      case gbk@VectorGroupByKey(vector) => emitValDef(sym, "%s.groupByKey".format(quote(vector)))
      case red@VectorReduce(vector, f) => emitValDef(sym, "%s.map(x => (x._1,x._2.reduce(%s)))".format(quote(vector), quote(red.closure)))
      case red@VectorReduceByKey(vector, f) => emitValDef(sym, "%s.reduceByKey(%s)".format(quote(vector), quote(red.closure)))
      case VectorSaves(saves) => stream.println("// save all vectors") 
      case GetArgs() => emitValDef(sym, "sparkInputArgs.drop(1); // First argument is for spark context")
    case _ => super.emitNode(sym, rhs)
  }
    println(sym+" "+rhs)
    out
    }
  
    override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: List[Exp[Any]])(body: List[TTP] => A): A = {
    val state = new TransformationState(currentScope0, result0)
    val transformer = new Transformer(state, List(new ReduceByKeyTransformation(), new MapMergeTransformation()))
//    buildGraph(transformer)
    transformer.doOneStep
    transformer.doOneStep
    System.err.println("Now starting to create closures")
    transformer.transformations = List(new MakeClosuresTransformation())
    transformer.doOneStep
    transformer.doOneStep
    transformer.doOneStep
    transformer.doOneStep
    transformer.doOneStep
//    buildGraph(transformer)
//    transformer.doOneStep
    //buildGraph(transformer)
    super.focusExactScopeFat(transformer.currentState.ttps)(transformer.currentState.results)(body)
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
package spark.examples;
import scala.math.random
import spark._
import SparkContext._

object %s {
        def main(sparkInputArgs: Array[String]) {
    		val sc = new SparkContext(sparkInputArgs(0), "%s")
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
