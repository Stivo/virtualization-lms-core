package scala.virtualization.lms
package epfl
package distributed

import common._
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


trait Utils extends Base with OverloadHack {
  
  def infix_+(a: Rep[String], b: Rep[Any])(implicit x: Overloaded1): Rep[String]
  def infix_+(a: Rep[Any], b: Rep[String])(implicit x: Overloaded2): Rep[String]
  def infix_+(a: String, b: Rep[Any])(implicit x: Overloaded4): Rep[String]
  def infix_+(a: Rep[Any], b: String)(implicit x: Overloaded5): Rep[String]
  
  implicit def unit(x:String): Rep[String]
  implicit def unit(x:Int): Rep[Int]
  
}


trait UtilExp extends BaseExp with Utils {

  implicit def unit(x:Int): Rep[Int] = Const(x)
  implicit def unit(x:String): Rep[String] = Const(x)
  
  def infix_+(a: Rep[String], b: Rep[Any])(implicit x: Overloaded1): Rep[String] = StrCat(a,b)
  def infix_+(a: Rep[Any], b: Rep[String])(implicit x: Overloaded2): Rep[String] = StrCat(a,b)
  def infix_+(a: String, b: Rep[Any])(implicit x: Overloaded4): Rep[String] = StrCat(Const(a),b)
  def infix_+(a: Rep[Any], b: String)(implicit x: Overloaded5): Rep[String] = StrCat(a,Const(b))

  case class StrCat(a: Exp[Any],b: Exp[Any]) extends Def[String]

  case class Tup[A,B](a: Exp[A],b: Exp[B]) extends Def[(A,B)]
  
  case class External[A:Manifest](s: String, fmt_args: List[Exp[Any]] = List()) extends Exp[A]
  
}

trait ScalaGenUtil extends ScalaGenBase {
  val IR: UtilExp
  import IR._
  
  // case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StrCat(a,b) =>
      emitValDef(sym, quote(a) + ".toString + " + quote(b) + ".toString")
    case Tup(a,b) =>
      emitValDef(sym, "("+ quote(a) + "," + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) = x match {
    case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
    case _ => super.quote(x)
  }


}

trait Vector[A] {
  //val elementType = manifest[A]
  //val elementType : Class[A]

}

trait VectorBase extends Base
  with Equal with IfThenElse with Variables with While with Functions
  with ImplicitOps with NumericOps with OrderingOps with StringOps
  with BooleanOps with PrimitiveOps with MiscOps with TupleOps
  with MathOps with CastingOps with ObjectOps with ArrayOps

trait VectorBaseExp extends VectorBase with UtilExp
  with DSLOpsExp
  with EqualExp with IfThenElseExp with VariablesExp with WhileExp with FunctionsExp
  with ImplicitOpsExp with NumericOpsExp with OrderingOpsExp with StringOpsExp
  with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp with TupleOpsExp
  with MathOpsExp with CastingOpsExp with ObjectOpsExp with ArrayOpsExp with RangeOpsExp
  with StructExp

//trait VectorBaseCodeGenPkg extends ScalaGenDSLOps
//  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenVariables with ScalaGenWhile with ScalaGenFunctions
//  with ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenOrderingOps //with ScalaGenStringOps
//  with ScalaGenBooleanOps with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps
//  with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenObjectOps with ScalaGenArrayOps with ScalaGenRangeOps
//  with ScalaGenStruct
//  { val IR: VectorOpsExp }


trait VectorOps extends VectorBase {
  //this: SimpleVector =>
	//syntax
    object Vector {
      def apply[A : Manifest](file : Rep[String]) = vector_new(file)
    }
    implicit def repVecToVecOps[A:Manifest](vector: Rep[Vector[A]]) = new vecOpsCls(vector)
//    implicit def repVecToVecTupleOps[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) = new vecTupleOpsCls(x)
    class vecOpsCls[A:Manifest](vector: Rep[Vector[A]]) {
    	def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(vector,f)
//		def filter(f: Rep[A] => Rep[Boolean]) = vector_filter(vector,f)
		def save(path : Rep[String]) = vector_save(vector, path)
    }

//    class vecTupleOpsCls[K: Manifest, V : Manifest](x: Rep[Vector[(K,V)]]) {
//      def reduceByKey(f : (Rep[V], Rep[V]) => Rep[V] ) = vector_reduceByKey[K, V](x, f)
//    }
    

    //operations
    def vector_new[A:Manifest](file : Rep[String]): Rep[Vector[A]]
    def vector_map[A : Manifest, B : Manifest](vector : Rep[Vector[A]], f : Rep[A] => Rep[B]) : Rep[Vector[B]]
//    def vector_filter[A : Manifest](vector : Rep[Vector[A]], f: Rep[A] => Rep[Boolean]) : Rep[Vector[A]]
    def vector_save[A : Manifest](vector : Rep[Vector[A]], path : Rep[String]) : Rep[Unit]
//    def vector_reduceByKey[K: Manifest, V : Manifest](vector : Rep[Vector[(K,V)]], f : (Rep[V], Rep[V]) => Rep[V] ) : Rep[Vector[(K, V)]]

}


trait VectorOpsExp extends VectorOps with VectorBaseExp {
    case class NewVector[A : Manifest](file : Exp[String]) extends Def[Vector[A]] {
      val mA = manifest[A]
    }
    
    case class VectorMap[A : Manifest, B : Manifest](in : Exp[Vector[A]], func : Exp[A] => Exp[B]) //, convert : Exp[Int] => Exp[A])
       extends Def[Vector[B]] {
      val mA = manifest[A]
      val mB = manifest[B]
    }

    case class VectorSave[A : Manifest](vector : Exp[Vector[A]], path : Rep[String]) extends Def[Unit] {
      val mA = manifest[A]
    }
    
//    case class VectorReduceByKey[K: Manifest, V : Manifest](vector : Exp[Vector[(K,V)]], f : (Exp[V], Exp[V]) => Exp[V]) extends Def[Vector[(K, V)]]
    
    override def vector_new[A: Manifest](file : Exp[String]) = NewVector[A](file)
    override def vector_map[A : Manifest, B : Manifest](vector : Exp[Vector[A]], f : Exp[A] => Exp[B]) = VectorMap[A, B](vector, f)
    override def vector_save[A : Manifest](vector : Exp[Vector[A]], file : Exp[String]) = reflectEffect(VectorSave[A](vector, file))

}

trait VectorImplOps extends VectorOps with FunctionsExp with UtilExp {
  
}

trait ScalaGenVector extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._
    override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
      case nv@NewVector(filename) => emitValDef(sym, "New vector created from %s with type %s".format(filename, nv.mA))
      case vs@VectorSave(vector, filename) => stream.println("Saving vector %s (of type %s) to %s".format(vector, vs.mA, filename))
      case vm@VectorMap(vector, function) => emitValDef(sym, "mapping vector %s with function %s".format(vector, function))
    case _ => super.emitNode(sym, rhs)
  }
}

trait HadoopGen extends ScalaGenBase with ScalaGenFunctions with ScalaGenUtil with ScalaGenVector {
  //val IR: dsl.type = dsl
  val IR: VectorOpsExp
  import IR._
  
  def getInputs(x : Any) : List[Int] = x match {
    case VectorMap(Sym(x), _) => List(x)
    case Reflect(VectorSave(Sym(x), _),_,_) => List(x)
    case _ => Nil
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
//	System.out.println("%s to %s".format(sym, rhs))
    
	for (x <- getInputs(rhs)) {
	  FileOutput.writeln("%s -> %s".format(x, sym.id))
	}
	super.emitNode(sym, rhs)
//	System.out.println("%s from inputs %s".format(sym, getInputs(rhs)))
  }
  
}

object FileOutput {
  val fw = new FileWriter("test.dot")
  fw.write("digraph g {\n")
  val sw = new StringWriter()
  def writeln(s : String) {
    fw.write(s+"\n")
    fw.flush
    sw.write(s+"\n")
  }
}
trait VectorsProg extends VectorImplOps {
  
  def test(x: Rep[Unit]) = {
    //RandomVector(7) + (ZeroVector(7) + RandomVector(7))
    val v1 = Vector[Int]("hello")
    val v2 = Vector[String]("hi2")
    val v3 = Vector[Boolean]("asdf")
//    v1.map(_+1)
    v2.save("test")
//    v1.map(x : Int => "")
    v3.map(!_)
    v2.map(_.startsWith("asdf")).map(!_)
  }
  
}

//trait StringsProg extends Vectors {
//  
//  def test(x: Rep[Any]) = {
//    val s: Rep[Any] = "hi " + "yo " + x + " done"
//    s
//  }
//  
//}
//



class TestVectors extends FileDiffSuite {
  
  def testVectors {
//    withOutput(System.err/*"test-out/epfl/test-dist"*/) {    
      println("-- begin")

      val dsl = new VectorsProg with VectorOpsExp with VectorImplOps 
//      val codegen = new ScalaGenFunctions with ScalaGenUtil with ScalaGenVector with ScalaGenBase { val IR: dsl.type = dsl }
//      codegen.emitSource(dsl.test, "g", new PrintWriter(System.out))
      val codegenDeps = new HadoopGen { val IR: dsl.type = dsl }
      codegenDeps.emitSource(dsl.test, "g", new PrintWriter(System.out))
      val graph = new GraphVizExport { val IR: dsl.type = dsl }
//      val r = { val x = fresh; x+1 }
//      graph.emitDepGraph(r, "test.dot")
//      new StringsProg with VectorsExp with VectorsImplExternal
//      with CompileScala { self =>
//        val codegen = new ScalaGenFunctions with ScalaGenUtil { val IR: self.type = self }
//        codegen.emitSource(test, "Test", new PrintWriter(System.out))
//        val g = compile(test)
//        println(g(0))
//      }

      println("-- end")
//    }
      println(FileOutput.sw.toString)
      
    //assert(true, "did finish")    
  }
}