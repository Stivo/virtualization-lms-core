package scala.virtualization.lms
package epfl
package distributed

import scala.virtualization.lms.common.ScalaGenBase
import java.io.PrintWriter
import scala.reflect.SourceContext

trait SparkProgram extends VectorOpsExp with VectorImplOps with SparkVectorOpsExp {
  
}

trait SparkVectorOpsExp extends VectorOpsExp {
   case class VectorReduceByKey[K: Manifest, V : Manifest](in : Exp[Vector[(K,V)]], func : (Exp[V], Exp[V]) => Exp[V]) 
    	extends Def[Vector[(K, V)]]{ 
    //with TypedNode [(K, Iterable[V]),(K,V)]{
      val mKey = manifest[K]
      val mValue = manifest[V]
      lazy val closure = doLambda2(func)(getClosureTypes._2, getClosureTypes._2, getClosureTypes._2)
      def getClosureTypes = (manifest[(V,V)], manifest[V])
      
//      def getTypes = (mKey, mValue)
    }

    override def syms(e: Any): List[Sym[Any]] = e match { 
    case s: ClosureNode[_,_] => syms(s.in, s.closure)// ++ super.syms(e) // super call: add case class syms (iff flag is set)
//    case s: ClosureNode[_,_] => syms(s.in)// ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case red : VectorReduceByKey[_,_] => syms(red.in, red.closure)
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
    case s: ClosureNode[_,_] => freqNormal(s.in)++freqHot(s.closure) 
    case red : VectorReduceByKey[_,_] => freqHot(red.closure) ++ freqNormal(red.in)
//    case s: ClosureNode[_,_] => freqNormal(s.in) 
//    case s: ClosureNode[_,_]  => freqHot(s.closure) ++ freqNormal(s.in) 
    case s: VectorReduce[_,_]  => freqHot(s.closure) ++ freqNormal(s.in) 
    case _ => super.symsFreq(e)
  }
  
      override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = {
        (e match {
        	case v@VectorReduceByKey(vector, func) => toAtom(VectorReduceByKey(f(vector), f(func))(v.mKey, v.mValue))
        	case _ => super.mirror(e, f)
        }).asInstanceOf[Exp[A]]
      }
}

//trait SparkTransformations extends VectorTransformations {
//    val IR: VectorOpsExp with SparkVectorOpsExp
//  	import IR.{VectorReduceByKey, VectorReduce, VectorGroupByKey, VectorMap}
//    import IR.{Def, Exp}
//
//  	class ReduceByKeyTransformation extends SimpleSingleConsumerTransformation {
//  	  
//	   def doTransformationPure(inExp : Exp[_]) = inExp match {
//            case Def(red@VectorReduce(Def(gbk@VectorGroupByKey(v1)),f1)) => {
//              new VectorReduceByKey(v1, f1)(red.mKey, red.mValue)
//            }
//            case _ => null
//	   }
//	}
//
//  	class PullSparkDependenciesTransformation extends PullDependenciesTransformation {
//
// 	   override def appliesToNodeImpl(inExp : Exp[_], t : Transformer) = {
//		   inExp match {
//		     case Def(VectorReduceByKey(in, func)) => true
//		     case _ => super.appliesToNodeImpl(inExp, t)
//		   }
//	   }
// 	   override def doTransformation(inExp : Exp[_]) : Def[_] = inExp match {
//	     case Def(r@VectorReduceByKey(in, func)) => _doneNodes += inExp; r
// 	     case _ => super.doTransformation(inExp)
// 	   }
//	}
//
//  	
//}

trait SparkGenVector extends ScalaGenBase with ScalaGenVector with VectorTransformations {
  
	val IR: SparkVectorOpsExp
	import IR.{Sym, Def, Exp, Reify, Reflect, Const}
	import IR.{NewVector, VectorSave, VectorMap, VectorFilter, VectorFlatMap, VectorFlatten, VectorGroupByKey, VectorReduce
	  , ComputationNode, VectorNode}
	import IR.{TTP, TP, SubstTransformer, ThinDef, Field}
	import IR.{findDefinition}
	import IR.{ClosureNode, freqHot, freqNormal, Lambda}

	import IR.{GetArgs}
	import IR.{VectorReduceByKey}
	import IR.{findDefinition, fresh, reifyEffects, reifyEffectsHere,toAtom}
		
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = 
    {val out = rhs match {
      case IR.SimpleStruct(tag, elems) => emitValDef(sym, "Creating struct with %s and elems %s".format(tag, elems)) 
      case nv@NewVector(filename) => emitValDef(sym, "sc.textFile(%s)".format(quote(filename)))
      case vs@VectorSave(vector, filename) => stream.println("%s.saveAsTextFile(%s)".format(quote(vector), quote(filename)))
      case vm@VectorMap(vector, function) => emitValDef(sym, "%s.map(%s)".format(quote(vector), quote(vm.closure)))
      case vf@VectorFilter(vector, function) => emitValDef(sym, "%s.filter(%s)".format(quote(vector), quote(vf.closure)))
      case vm@VectorFlatMap(vector, function) => emitValDef(sym, "%s.flatMap(%s)".format(quote(vector), quote(vm.closure)))
      case vm@VectorFlatten(v1) => {
        var out = "("+v1.map(quote(_)).mkString(").union(")
        out += ")"
        emitValDef(sym, out)
      }
      case gbk@VectorGroupByKey(vector) => emitValDef(sym, "%s.groupByKey".format(quote(vector)))
      case red@VectorReduce(vector, f) => emitValDef(sym, "%s.map(x => (x._1,x._2.reduce(%s)))".format(quote(vector), quote(red.closure)))
      case red@VectorReduceByKey(vector, f) => emitValDef(sym, "%s.reduceByKey(%s)".format(quote(vector), quote(red.closure)))
      case GetArgs() => emitValDef(sym, "sparkInputArgs.drop(1); // First argument is for spark context")
    case _ => super.emitNode(sym, rhs)
  }
//    println(sym+" "+rhs)
    out
    }
  
  object TTPDef {
    def unapply(ttp : TTP) = ttp match {
      case TTP(_, ThinDef(x)) => Some(x)
      case _ => None
    }
  }
  
  object FieldAccess {
    def unapply(ttp : TTP) = ttp match {
      case TTPDef(f@Field(obj,field,typ)) => Some(f)
      case _ => None
    }
  }
  
  object ClosureNode {
    def unapply(any : Any) = any match {
      case TTPDef(cn : ClosureNode[_,_]) => Some(cn)
      case cn : ClosureNode[_,_] => Some(cn)
      case _ => None
    }
  }
  
    override def focusExactScopeFat[A](currentScope0In: List[TTP])(result0B: List[Block[Any]])(body: List[TTP] => A): A = {
      var result0 = result0B.map(getBlockResultFull)
      var state = new TransformationState(currentScope0In, result0)
      val transformer = new Transformer(state, List(new MapMergeTransformation()))
////    buildGraph(transformer)
    transformer.stepUntilStable(50)
    transformer.transformations = List(new PullDependenciesTransformation())
    transformer.stepUntilStable(50)
//    buildGraph(transformer)
//    transformer.doOneStep
    //buildGraph(transformer)
      state = transformer.currentState
      val currentScope0 = state.ttps
      result0 = state.results
    
      def getValidOptions[A](l : Iterable[Option[A]]) = l.filter(_.isDefined).map(_.get)
      
	  val closureNodes = getValidOptions[ClosureNode[_,_]](currentScope0.map(ClosureNode.unapply))
	  def vectorNodes = getValidOptions[VectorNode](currentScope0.map{x => x match {case TTPDef(vn : VectorNode) => Some(vn); case _ => None}})
      def findClosureNodeWithLambda(l : Lambda[_,_]) = {
//        val closureNodes = getValidOptions[ClosureNode[_,_]](currentScope0.map{_ match { case TTP(_,ThinDef(cn : ClosureNode[_,_])) => Some(cn); case _ => None}})
        val sym = IR.findDefinition(l).get.sym
        closureNodes.find(IR.syms(_).contains(sym))
      }
      
      def findLambdaForSym(x : Sym[_]) = {
    	  currentScope0.map{ x : TTP => x match {case TTP(_,ThinDef(l : IR.Lambda[_,_])) => Some(l); case _ => None}}
    	  .filter(_.isDefined).map(_.get).find(_.x == x)
      }
      
      def findClosureNode(node : Def[_], scope : List[TTP]) = {
        for (reading <- IR.syms(node);
            lambdaOpt <- findLambdaForSym(reading);
            cn <- findClosureNodeWithLambda(lambdaOpt))
          yield cn
      }.toList
      
      println("New current scope with result "+result0)
//      currentScope0.foreach(println)
      val accesses = currentScope0.flatMap{FieldAccess.unapply(_)}
      val tuples = accesses.map(x => (x,findClosureNode(x, currentScope0)))
      tuples.foreach{case (read, closurenode::Nil) => closurenode.directFieldReads += read.index; case _ => }
      println("Direct reads of closure nodes ")
      closureNodes.foreach(x => println(x+" "+x.directFieldReads))
      
      def addFieldReadsToPredecessors( in : VectorNode) {
        val inputSyms = IR.syms(in)
        val nodes = inputSyms.flatMap{IR.findDefinition(_)}.map(_.rhs).map{x => println("GUGUS "+x); x}.flatMap{_ match {case vn : VectorNode => Some(vn); case _ => None}}
        nodes.foreach{y => y.successorFieldReads ++= in.directFieldReads; addFieldReadsToPredecessors(y)}
      }
      
      println(vectorNodes)
      vectorNodes.foreach{addFieldReadsToPredecessors}
      println("All reads of vector nodes ")
      vectorNodes.foreach{x => println(x+" "+x.allFieldReads)}
      println
    super.focusExactScopeFat(currentScope0)(result0.map(IR.Block(_)))(body)
  }
  
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
//    val func : Exp[A] => Exp[B] = {x => reifyEffects(f(x))}
    
    val x = fresh[A]
    val y = reifyBlock(f(x))

    val sA = mA.toString
    val sB = mB.toString

//    val staticData = getFreeDataBlock(y)

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
    
//    staticData
    Nil
  }

    
}

trait SparkGen extends VectorBaseCodeGenPkg with SparkGenVector
