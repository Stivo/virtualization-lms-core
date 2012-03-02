package scala.virtualization.lms
package epfl.distributed

import scala.virtualization.lms.common.ScalaGenBase
import scala.collection.mutable

trait VectorTransformations extends ScalaGenBase with ScalaGenVector {
	//val IR: dsl.type = dsl
    val IR: VectorOpsExp
	import IR.{Sym, Def, Exp, Reify, Reflect, Const}
	import IR.{NewVector, VectorSave, VectorMap, VectorFilter, VectorFlatMap, VectorFlatten, VectorGroupByKey, VectorReduce, 
	  ComputationNode, VectorSaves}
	import IR.{TTP, TP, SubstTransformer, IRNode, ThinDef}
	import IR.{findDefinition}
	import IR.{ClosureNode, freqHot, freqNormal, Lambda}
	
	class MarkerTransformer( val transformation : Transformation, val transformer : Transformer) extends SubstTransformer {
		private var toDo = mutable.HashSet[Exp[_]]()
		override def apply[A](inExp: Exp[A]) : Exp[A] = {
		  if (transformation.appliesToNode(inExp, transformer)) {
		    toDo += inExp
		  }
		  inExp
		}
		def getTodo = toDo.toSet
	}
	
	class Transformer(var currentState : TransformationState, var transformations : List[Transformation]) {
	  def doOneStep = {
	    currentState.printAll
	    var out = false
		for (transformation <- transformations) {
		  val marker = new MarkerTransformer(transformation, this)
		  transformAll(marker)
		  for (exp <- marker.getTodo) {
		    out = true
		    System.out.println("Applying "+transformation+" to node "+exp)
		    val (ttps, substs) = transformation.applyToNode(exp, this)
		    val newState = new TransformationState(currentState.ttps ++ ttps, currentState.results)
		    val subst = new SubstTransformer()
		    subst.subst ++= substs
		    currentState = transformAll(subst, newState)
		  }
		}
	    currentState.printAll
	    out
	  }
		
		def transformAll(marker : SubstTransformer, state : TransformationState = currentState) = {
		  val after = transformAllFully(state.ttps, state.results, marker) match {
		    case (scope, results) => new TransformationState(scope, results)
		    case _ => state
		  }
		  after
		}
	
		def readingNodes (inExp : Def[_])= {
		  val reading = IR.syms(inExp).removeDuplicates
		  val out = reading.flatMap{ x=> IR.findDefinition(x)}.map(_.rhs)
//		  println("Reading nodes of "+inExp+" are "+reading+", with Defs "+out)
		  out
		}
		def getConsumers (inExp : Exp[_]) = {
		  val inSym = inExp.asInstanceOf[Sym[_]]
		  val out = currentState.ttps.flatMap {
		    _.rhs match {
		      case ThinDef(x) => Some(x)
		      case _ => None
		    }
		  }.filter(IR.findDefinition(_).isDefined)
		  .filter{x => IR.syms(x).contains(inSym)}.removeDuplicates
		  out
		}
		
	}
	
	class TransformationState(val ttps : List[TTP], val results : List[Exp[Any]]) {
	  def printAll = {
	    println("Printing all ttps for the current state")
	    ttps.foreach(println)
	  }
	}
	
	trait Transformation {
	  
	  def appliesToNode(inExp : Exp[_], t : Transformer) : Boolean
	  
	  def applyToNode(inExp : Exp[_], transformer : Transformer) = {
		  val out = doTransformation(inExp);
		  // get dependencies
		  val readers = transformer.readingNodes(out)
		  // find all new Defs
		  var newDefs = List(out)++readers
//		  newDefs = newDefs.flatMap(x => transformer.readingNodes(x)++List(x))
//		  newDefs = newDefs.flatMap(transformer.readingNodes)
//		  newDefs = newDefs.flatMap(transformer.readingNodes)
		  newDefs = newDefs.removeDuplicates
		  println(" the new newDefs")
		  println(newDefs)
		  // make TTP's from defs
		  val ttps = newDefs.map(IR.findOrCreateDefinition(_,Nil)).map(fatten)
		  // return ttps and substitutions
		  (ttps, List((inExp, IR.findOrCreateDefinition(out,Nil).sym))) 
	  }
	  def doTransformation(inExp : Exp[_]) :Def[_]
		
	  override def toString = 
	    this.getClass.getSimpleName.replaceAll("Transformation","")
	    .split("(?=[A-Z])").mkString(" ")
	  
	}
	
    class SinkFlattenTransformation extends Transformation {
	   def appliesToNode(inExp : Exp[_], t : Transformer) : Boolean = {
	       inExp match {
            case Def(vm@VectorMap(Def(vf@VectorFlatten(v1, v2)),func)) => true
            case _ => false
	      }
	   }
	   def doTransformation(inExp : Exp[_]) = inExp match {
            case Def(vm@VectorMap(Def(vf@VectorFlatten(v1, v2)),func)) => {
              val mapLeft = new VectorMap(v1,func)
              val mapRight = new VectorMap(v2,func)
              val defLeft = IR.toAtom2(mapLeft)
              val defRight = IR.toAtom2(mapRight)
              new VectorFlatten(defLeft, defRight)
            }
           // match error is ok, should not happen
	   }

	}
	
	trait SimpleTransformation extends Transformation {
	  override def appliesToNode(inExp : Exp[_], t : Transformer) : Boolean = {
	    doTransformationPure(inExp) != null
	  }
	  final def doTransformation(inExp : Exp[_]) = {
	    doTransformationPure(inExp)
	  }
	  def doTransformationPure(inExp : Exp[_]) : Def[_]
	}
	
	trait SingleConsumerTransformation extends Transformation {
	  override final def appliesToNode(inExp : Exp[_], t : Transformer) : Boolean = {
	    if (!appliesToNodeImpl(inExp, t))
	      false 
        else {
	      val out = t.readingNodes(IR.findDefinition(inExp.asInstanceOf[Sym[_]]).get.rhs)
	      //.flatMap(_).fold(true)()
	      out.map(x => t.getConsumers(IR.findDefinition(x).get.sym).size == 1).fold(true)(_&&_)
        }
	  }
	  def appliesToNodeImpl(inExp : Exp[_], t : Transformer) : Boolean 
	}
	
    trait SimpleSingleConsumerTransformation extends SimpleTransformation with SingleConsumerTransformation {
    	  override def appliesToNodeImpl(inExp : Exp[_], t : Transformer) : Boolean = {
    		  doTransformationPure(inExp) != null
    	  }
    }

	
	class MergeMapsTransformation extends SimpleTransformation {
	   def doTransformationPure(inExp : Exp[_]) = inExp match {
            case Def(m1@VectorMap(Def(m2@VectorMap(v1, f2)),f1)) => {
              new VectorMap(v1, f2.andThen(f1))
            }
            case _ => null
	   }
	}

}