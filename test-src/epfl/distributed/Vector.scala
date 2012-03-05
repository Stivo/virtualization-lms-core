package scala.virtualization.lms
package epfl
package distributed

import common._
import scala.virtualization.lms.internal._
import test1._
import util.OverloadHack
import java.io.PrintWriter
import java.io.FileOutputStream
import java.io.PrintWriter
import scala.collection.mutable.HashMap
import internal.GraphVizExport
import java.io.File
import java.io.FileWriter
import java.io.StringWriter
import scala.reflect.SourceContext
import scala.collection.mutable.Buffer

trait Vector[A] {

}

trait VectorBase extends Base with LiftAll
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with NumericOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with ArrayOps
  with StringAndNumberOps

trait VectorBaseExp extends VectorBase
  with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with NumericOpsExp with OrderingOpsExp with StringOpsExp
  with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with MathOpsExp with CastingOpsExp with ObjectOpsExp with ArrayOpsExp with RangeOpsExp
  with StructExp with FatExpressions with LoopsFatExp with IfThenElseFatExp
  with StringAndNumberOpsExp
  
trait VectorBaseCodeGenPkg extends ScalaGenDSLOps
  with SimplifyTransform
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
  with ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenOrderingOps with ScalaGenStringOps
  with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
  with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenObjectOps with ScalaGenArrayOps with ScalaGenRangeOps
  with ScalaGenStruct with GenericFatCodegen
  with StringAndNumberOpsCodeGen
  { val IR: VectorOpsExp }


trait VectorOps extends VectorBase {
  //this: SimpleVector =>
	//syntax
    object Vector {
      def apply(file : Rep[String]) = vector_new[String](file)
      def getArgs = get_args()
    }
    
    implicit def repVecToVecOps[A:Manifest](vector: Rep[Vector[A]]) = new vecOpsCls(vector)
    class vecOpsCls[A:Manifest](vector: Rep[Vector[A]]) {
        def flatMap[B : Manifest](f : Rep[A] => Rep[Iterable[B]]) = vector_flatMap(vector, f)
    	def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(vector,f)
		def filter(f: Rep[A] => Rep[Boolean]) = vector_filter(vector,f)
		def save(path : Rep[String]) = vector_save(vector, path)
		def ++(vector2 : Rep[Vector[A]]) = vector_++(vector, vector2)
    }

    implicit def repVecToVecIterableTupleOpsCls[K: Manifest, V : Manifest](x: Rep[Vector[(K,Iterable[V])]]) = new vecIterableTupleOpsCls(x)
    class vecIterableTupleOpsCls[K: Manifest, V : Manifest](x: Rep[Vector[(K,Iterable[V])]]) {
      def reduce(f : (Rep[V], Rep[V]) => Rep[V] ) = vector_reduce[K, V](x, f)
    }
    
    implicit def repVecToVecTupleOps[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) = new vecTupleOpsCls(x)
    class vecTupleOpsCls[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) {
      def groupByKey = vector_groupByKey[K, V](x)
    }

   def get_args() : Rep[Array[String]]
   
    //operations
    def vector_new[A:Manifest](file : Rep[String]): Rep[Vector[String]]
    def vector_map[A : Manifest, B : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[B]) : Rep[Vector[B]]
    def vector_flatMap[A : Manifest, B : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[Iterable[B]]) : Rep[Vector[B]]
    def vector_filter[A : Manifest](vector : Rep[Vector[A]], f: Rep[A] => Rep[Boolean]) : Rep[Vector[A]]
    def vector_save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) : Rep[Unit]
    def vector_++[A : Manifest](vector1 : Rep[Vector[A]], vector2 : Rep[Vector[A]]) : Rep[Vector[A]]
    def vector_reduce[K: Manifest, V : Manifest](vector : Rep[Vector[(K,Iterable[V])]], f : (Rep[V], Rep[V]) => Rep[V] ) : Rep[Vector[(K, V)]]
    def vector_groupByKey[K: Manifest, V : Manifest](vector : Rep[Vector[(K,V)]]) : Rep[Vector[(K, Iterable[V])]]
}

object FakeSourceContext {
  def apply() = SourceContext("unknown", Nil)
}

trait VectorOpsExp extends VectorOps with VectorBaseExp with FunctionsExp {
  def toAtom2[T:Manifest](d: Def[T])(implicit ctx: SourceContext): Exp[T] = super.toAtom(d)
  
	trait ClosureNode[A, B] {
      val in : Exp[Vector[_]]
	  val func : Exp[A] => Exp[B]
	  def getClosureTypes : (Manifest[A], Manifest[B])
	  
	  lazy val closure = {
        VectorOpsExp.this.doLambda(func)(getClosureTypes._1, getClosureTypes._2, FakeSourceContext()) 
      }

	}
  
	trait ComputationNode {
	  def getTypes : (Manifest[_], Manifest[_])
	}
  
	trait ComputationNodeTyped[A, B] extends ComputationNode {
	  override def getTypes : (Manifest[A], Manifest[B])
	}
	
	trait PreservingTypeComputation[A] extends ComputationNodeTyped[A,A]{
	  def getType : Manifest[A]
	  def getTypes = (getType, getType)
	}

    case class NewVector[A : Manifest](file : Exp[String]) extends Def[Vector[String]]
    		with ComputationNodeTyped[Nothing, Vector[A]] {
      val mA = manifest[A]
      def getTypes = (manifest[Nothing], manifest[Vector[A]])
    }
    
    def makeVectorManifest[B : Manifest] = manifest[Vector[B]]
    
    case class VectorMap[A : Manifest, B : Manifest](in : Exp[Vector[A]], func : Exp[A] => Exp[B]) //, convert : Exp[Int] => Exp[A])
       extends Def[Vector[B]] with ComputationNodeTyped[Vector[A],Vector[B]] with ClosureNode[A, B]{
   	  val mA = manifest[A]
      val mB = manifest[B]
   	  def getClosureTypes = (mA, mB)
   	  def getTypes = (manifest[Vector[A]], manifest[Vector[B]])
//      val closure : Exp[A => B] = VectorOpsExp.this.doLambda(func)(mA, manifest[B], SourceContext("unknown", Nil))
    }

    case class VectorFilter[A : Manifest](in : Exp[Vector[A]], func : Exp[A] => Exp[Boolean])
       extends Def[Vector[A]] with PreservingTypeComputation[Vector[A]] with ClosureNode[A, Boolean] {
      val mA = manifest[A]
      def getClosureTypes = (mA, Manifest.Boolean)
      def getType = manifest[Vector[A]]
    }
    
    case class VectorFlatMap[A : Manifest, B : Manifest](in : Exp[Vector[A]], func : Exp[A] => Exp[Iterable[B]]) //, convert : Exp[Int] => Exp[A])
       extends Def[Vector[B]] with ComputationNodeTyped[Vector[A],Vector[B]] with ClosureNode[A, Iterable[B]] {
      val mA = manifest[A]
      val mB = manifest[B]
      def getTypes = (manifest[Vector[A]], manifest[Vector[B]])
      def getClosureTypes = (manifest[A], manifest[Iterable[B]])
    }
   
    case class VectorFlatten[A : Manifest](vectors : List[Exp[Vector[A]]]) extends Def[Vector[A]] 
    		with PreservingTypeComputation[Vector[A]] {
      val mA = manifest[A]
      def getType = manifest[Vector[A]]
    }

    case class VectorGroupByKey[K : Manifest, V : Manifest](v1 : Exp[Vector[(K,V)]]) extends Def[Vector[(K, Iterable[V])]] {
//    		with TypedNode [(K,V),(K,Iterable[V])]{
      val mKey = manifest[K]
      val mValue = manifest[V]
      val mOutType = manifest[(K,Iterable[V])]
//      def getTypes = (manifest[(K,V)], mOutType)
    }
    
    case class VectorReduce[K: Manifest, V : Manifest](in : Exp[Vector[(K,Iterable[V])]], func : (Exp[V], Exp[V]) => Exp[V]) 
    	extends Def[Vector[(K, V)]]{ 
    //with TypedNode [(K, Iterable[V]),(K,V)]{
      val mKey = manifest[K]
      val mValue = manifest[V]
      lazy val closure = doLambda2(func)(getClosureTypes._2, getClosureTypes._2, getClosureTypes._2, FakeSourceContext())
      def getClosureTypes = (manifest[(V,V)], manifest[V])
      
//      def getTypes = (mKey, mValue)
    }
    
    case class VectorSave[A : Manifest](vectors : Exp[Vector[A]], path : Exp[String]) extends Def[Unit]
    		with ComputationNodeTyped[Vector[A], Nothing]{
    	val mA = manifest[A]
    	def getTypes = (manifest[Vector[A]], manifest[Nothing])
    }
    
    case class GetArgs extends Def[Array[String]]
    
    override def get_args() = GetArgs()
    override def vector_new[A: Manifest](file : Exp[String]) = NewVector[A](file)
    override def vector_map[A : Manifest, B : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[B]) = VectorMap[A, B](vector, f)
    override def vector_flatMap[A : Manifest, B : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[Iterable[B]]) = VectorFlatMap(vector, f)
    override def vector_filter[A : Manifest](vector : Rep[Vector[A]], f: Exp[A] => Exp[Boolean]) = VectorFilter(vector, f)
    override def vector_save[A : Manifest](vector : Exp[Vector[A]], file : Exp[String]) = {
      val save = new VectorSave[A](vector, file)
      reflectEffect(save)
    }
    override def vector_++[A : Manifest](vector1 : Rep[Vector[A]], vector2 : Rep[Vector[A]]) = VectorFlatten(List(vector1, vector2))
    override def vector_reduce[K: Manifest, V : Manifest](vector : Exp[Vector[(K,Iterable[V])]], f : (Exp[V], Exp[V]) => Exp[V] ) = VectorReduce(vector, f)
    override def vector_groupByKey[K: Manifest, V : Manifest](vector : Exp[Vector[(K,V)]]) = VectorGroupByKey(vector)
    
    override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
        e match {
        case flat@VectorFlatten(list) => toAtom(VectorFlatten(f(list))(flat.mA))
        case vm@NewVector(vector) => toAtom(NewVector(f(vector))(vm.mA))(mtype(vm.mA), implicitly[SourceContext])
	    case vm@VectorMap(vector,func) => toAtom(VectorMap(f(vector), f(func))(vm.mA, vm.mB))(vm.getTypes._2, implicitly[SourceContext])
	    case vf@VectorFilter(vector,func) => toAtom(VectorFilter(f(vector), f(func))(vf.mA))(mtype(manifest[A]), implicitly[SourceContext])
	    case vfm@VectorFlatMap(vector,func) => toAtom(VectorFlatMap(f(vector), f(func))(vfm.mA, vfm.mB))(mtype(manifest[A]), implicitly[SourceContext])
	    case gbk@VectorGroupByKey(vector) => toAtom(VectorGroupByKey(f(vector))(gbk.mKey, gbk.mValue))(mtype(manifest[A]), implicitly[SourceContext])
	    case v@VectorReduce(vector, func) => toAtom(VectorReduce(f(vector), f(func))(v.mKey, v.mValue))(mtype(manifest[A]), implicitly[SourceContext])
	    case vs@VectorSave(vector, path) => toAtom(VectorSave(f(vector), f(path))(vs.mA))
	    case Reflect(vs@VectorSave(vector, path), u, es) => reflectMirrored(Reflect(VectorSave(f(vector), f(path))(vs.mA), mapOver(f,u), f(es)))
	    case Reify(x, u, es) => toAtom(Reify(f(x), mapOver(f,u), f(es)))(mtype(manifest[A]), implicitly[SourceContext])
	    case _ => super.mirror(e,f)
	}}.asInstanceOf[Exp[A]]

    
  override def syms(e: Any): List[Sym[Any]] = e match { //TR TODO: question -- is alloc a dependency (should be part of result) or a definition (should not)???
                                                        // aks: answer -- we changed it to be internal to the op to make things easier for CUDA. not sure if that still needs
                                                        // to be the case. similar question arises for sync
    case s: ClosureNode[_,_]  => syms(s.in, s.closure) ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case VectorFlatten(x) => syms(x) ++ super.syms(e)
    case NewVector(arg) => syms(arg)
    case VectorSave(vec, path) => syms(vec, path)
//    case s: VectorMap[_,_]  => syms(s.func, s.in) ++ super.syms(e) // super call: add case class syms (iff flag is set)
    case _ => super.syms(e)
  }
    
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
     case s: ClosureNode[_,_]  => freqHot(s.closure) ++ freqNormal(s.in) 
     case VectorFlatten(x) => freqNormal(x)
    case NewVector(arg) => freqNormal(arg)
    case VectorSave(vec, path) => freqNormal(vec, path)
    case _ => super.symsFreq(e)
  }
  
	/////////////////////
  // aliases and sharing
  // TODO
    
}

trait VectorImplOps extends VectorOps with FunctionsExp  {
  
}

trait ScalaGenVector extends ScalaGenBase with GenericFatCodegen with SimplifyTransform with FatScheduling {
  val IR: VectorOpsExp
  import IR._
    override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
      case nv@NewVector(filename) => emitValDef(sym, "New vector created from %s with type %s".format(filename, nv.mA))
      case vs@VectorSave(vector, filename) => stream.println("Saving vector %s (of type %s) to %s".format(vector, vs.mA, filename))
      case vm@VectorMap(vector, function) => emitValDef(sym, "mapping vector %s with function %s, type %s => %s".format(vector, quote(vm.closure), vm.mA, vm.mB))
      case vf@VectorFilter(vector, function) => emitValDef(sym, "filtering vector %s with function %s".format(vector, function))
      case vm@VectorFlatMap(vector, function) => emitValDef(sym, "flat mapping vector %s with function %s".format(vector, function))
      case vm@VectorFlatten(v1) => emitValDef(sym, "flattening vectors %s".format(v1))
      case gbk@VectorGroupByKey(vector) => emitValDef(sym, "grouping vector by key")
      case red@VectorReduce(vector, f) => emitValDef(sym, "reducing vector")
      case GetArgs() => emitValDef(sym, "getting the arguments")
    case _ => super.emitNode(sym, rhs)
  }
}


object FileOutput {
  var x = 0
  def writeFile(s : String) = {
    val fw = new FileWriter("test"+x+".dot")
    fw.write(s)
    fw.close()
    x+=1
  }
  
  val fw = new FileWriter("test.dot")
  val sw = new StringWriter()
//  writeln("digraph {")
  def writeln(s : String) {
    fw.write(s)
    fw.flush
    sw.write(s)
  }
//  def finish { writeln("}")}
}