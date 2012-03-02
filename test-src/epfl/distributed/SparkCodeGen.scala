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


trait SparkGenVector extends ScalaGenBase with ScalaGenVector {
  
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
	
	def emitFunction[A : Manifest, B : Manifest](f : Exp[A] => Exp[B])(implicit stream: PrintWriter) {
	      val x = fresh[A]
	      val y = reifyEffects(f(x)) // unfold completely at the definition site. 
                               // TODO: this will not work if f is recursive. 
                               // need to incorporate the other pieces at some point.
     stream.println("val ffff  = {" + quote(x) + ": (" + x.Type + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + y.Type)
      stream.println("}")

//	  emitBlock(f)
	}
	
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = 
    {val out = rhs match {
      case nv@NewVector(filename) => emitValDef(sym, "sc.textFile(%s)".format(quote(filename)))
      case vs@VectorSave(vector, filename) => stream.println("%s.saveAsTextFile(%s)".format(quote(vector), quote(filename)))
      case vm@VectorMap(vector, function) => emitValDef(sym, "%s.map(%s)".format(quote(vector), emitFunction(function)))
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
    var result: List[Exp[Any]] = result0
    var currentScope = currentScope0
   
      class Transformer extends SubstTransformer {
      def addSubstitution(sym1 : Exp[_], sym2 : Exp[_]) {
        System.err.println("Adding substitution "+sym1+" = "+sym2)
        subst(sym1) = sym2
      }
        override def apply[A](inExp: Exp[A]) : Exp[A] = {
          val symdeps = inExp match {
            case Def(x) => IR.syms(x).mkString(", ")
            case _ => ""
          }
          System.err.print("mirroring "+inExp+" "+" symdeps ="+symdeps+"; ");
          if (subst.contains(inExp)) {
            return super.apply(inExp)
          }
          System.err.println(inExp match {
            case Def(y) => y
          	case _=> "not a def"
          })
          var replace : Exp[_]= null
          val out = (inExp match {
            
          case Def(vr@VectorReduce(Def(vg@VectorGroupByKey(in)), f)) => {
        	  val newNode = IR.toAtom2(IR.VectorReduceByKey(in, f))
              addSubstitution(inExp, newNode)
              newNode
            }
//          	case Def(v1@VectorMap(Def(v2@VectorMap(in, f1)), f2)) 
//          	if v2.reads <= 1 => {
//          	  replace = IR.syms(v1).apply(1)
//          	  def getInputSymbol(v : VectorMap[_,_]) = {
//	          	  val clos = v.closure match {
//	          	    case Def(l:Lambda[_,_]) => l
//	          	    case _ => null
//	          	  }
//	          	  clos.x
//          	  }
//          	  v1.alive = false
//          	  v2.alive = false
//          	  System.err.println("Merging VectorMaps "+v1+" "+v2)
//          	  val composed = VectorMap(in, f1.andThen(f2))(v2.mA, v1.mB)
//          	  
//          	  val out = IR.toAtom2(composed)
//          	  out match {
//          	    case Def(v : VectorMap[_,_]) => addSubstitution(getInputSymbol(v2),getInputSymbol(v))
//          	    case _ => 
//          	  }
//          	  out
//          	}
          	case _ => inExp
          }).asInstanceOf[Exp[A]]
          if (out != inExp) {
            addSubstitution(inExp, out)
            out match {
              case Def(vm@VectorMap(x,y)) => {
                addSubstitution(replace, vm.closure)
              }
              case _ =>
            }
            
          }
          super.apply(out)
        }
      }
	  val t = new Transformer()
	  // with fatschedule: deps(IR.syms(result)).map(_.rhs)
	  val (scope1, result1) = transformAllFully(currentScope0, result, t)
  	  super.focusExactScopeFat(scope1)(result1)(body)
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
