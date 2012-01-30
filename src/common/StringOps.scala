package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

trait LiftString {
  this: Base =>

  implicit def strToRepStr(s: String) = unit(s)
}

trait StringOps extends Variables with OverloadHack {
  // NOTE: if something doesn't get lifted, this won't give you a compile time error,
  //       since string concat is defined on all objects
  
  def infix_+(s1: String, s2: Rep[Any])(implicit o: Overloaded1, ctx: SourceContext) = string_plus(unit(s1), s2)
  def infix_+(s1: String, s2: Var[Any])(implicit o: Overloaded2, ctx: SourceContext) = string_plus(unit(s1), readVar(s2))  
  def infix_+(s1: Rep[String], s2: Rep[Any])(implicit o: Overloaded1, ctx: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[String], s2: Var[Any])(implicit o: Overloaded2, ctx: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[String], s2: Rep[String])(implicit o: Overloaded3, ctx: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[String], s2: Var[String])(implicit o: Overloaded4, ctx: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[Any], s2: Rep[String])(implicit o: Overloaded5, ctx: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[Any], s2: Var[String])(implicit o: Overloaded6, ctx: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[Any], s2: String)(implicit o: Overloaded7, ctx: SourceContext) = string_plus(s1, unit(s2))
  
  def infix_+(s1: Var[String], s2: Rep[Any])(implicit o: Overloaded8, ctx: SourceContext) = string_plus(readVar(s1), s2)  
  def infix_+(s1: Var[String], s2: Var[Any])(implicit o: Overloaded9, ctx: SourceContext) = string_plus(readVar(s1), readVar(s2))    
  def infix_+(s1: Var[String], s2: Rep[String])(implicit o: Overloaded10, ctx: SourceContext) = string_plus(readVar(s1), s2)    
  def infix_+(s1: Var[String], s2: Var[String])(implicit o: Overloaded11, ctx: SourceContext) = string_plus(readVar(s1), readVar(s2))    
  def infix_+(s1: Var[Any], s2: Rep[String])(implicit o: Overloaded12, ctx: SourceContext) = string_plus(readVar(s1), s2)  
  def infix_+(s1: Var[Any], s2: Var[String])(implicit o: Overloaded13, ctx: SourceContext) = string_plus(readVar(s1), readVar(s2))  
  def infix_+(s1: Var[Any], s2: String)(implicit o: Overloaded14, ctx: SourceContext) = string_plus(readVar(s1), unit(s2))
  
  // these are necessary to be more specific than arithmetic/numeric +. is there a more generic form of this that will work?
  //def infix_+[R:Manifest](s1: Rep[String], s2: R)(implicit c: R => Rep[Any], o: Overloaded15, ctx: SourceContext) = string_plus(s1, c(s2))  
  def infix_+(s1: Rep[String], s2: Double)(implicit o: Overloaded15, ctx: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Float)(implicit o: Overloaded16, ctx: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Int)(implicit o: Overloaded17, ctx: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Long)(implicit o: Overloaded18, ctx: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Short)(implicit o: Overloaded19, ctx: SourceContext) = string_plus(s1, unit(s2))  
  
  def infix_startsWith(s1: Rep[String], s2: Rep[String])(implicit ctx: SourceContext) = string_startswith(s1,s2)
  def infix_trim(s: Rep[String])(implicit ctx: SourceContext) = string_trim(s)
  def infix_split(s: Rep[String], separators: Rep[String])(implicit ctx: SourceContext) = string_split(s, separators, unit(0))
  def infix_split(s: Rep[String], separators: Rep[String], limit : Rep[Int])(implicit o : Overloaded1, ctx: SourceContext) = string_split(s, separators, limit)
  def infix_toDouble(s: Rep[String])(implicit ctx: SourceContext) = string_todouble(s)
  def infix_replaceAll(s : Rep[String], regex : Rep[String], rep : Rep[String])(implicit ctx: SourceContext) = string_replaceall(s, regex, rep)
  def infix_contains(s : Rep[String], substring : Rep[String])(implicit ctx: SourceContext) = string_contains(s, substring)
  def infix_isEmpty(s : Rep[String])(implicit ctx: SourceContext) = string_isempty(s)

  object String {
    def valueOf(a: Rep[Any])(implicit ctx: SourceContext) = string_valueof(a)
  }

  def string_plus(s: Rep[Any], o: Rep[Any])(implicit ctx: SourceContext): Rep[String]
  def string_startswith(s1: Rep[String], s2: Rep[String])(implicit ctx: SourceContext): Rep[Boolean]
  def string_trim(s: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def string_split(s: Rep[String], separators: Rep[String], limit : Rep[Int])(implicit ctx: SourceContext): Rep[Array[String]]
  def string_valueof(d: Rep[Any])(implicit ctx: SourceContext): Rep[String]
  def string_todouble(s: Rep[String])(implicit ctx: SourceContext): Rep[Double]
  def string_replaceall(s : Rep[String], regex : Rep[String], repl : Rep[String])(implicit ctx: SourceContext) : Rep[String]
  def string_contains(s : Rep[String], substring : Rep[String])(implicit ctx: SourceContext) : Rep[Boolean]
  def string_isempty(s : Rep[String])(implicit ctx: SourceContext) : Rep[Boolean]
}

trait StringOpsExp extends StringOps with VariablesExp {
  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringStartsWith(s1: Exp[String], s2: Exp[String]) extends Def[Boolean]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String], limit : Exp[Int]) extends Def[Array[String]]
  case class StringValueOf(a: Exp[Any]) extends Def[String]
  case class StringToDouble(s: Exp[String]) extends Def[Double]
  case class StringReplaceAll(s: Exp[String], regex : Exp[String], rep : Exp[String]) extends Def[String]
  case class StringContains(s: Exp[String], substring : Exp[String]) extends Def[Boolean]
  case class StringIsEmpty(s: Exp[String]) extends Def[Boolean]

  def string_plus(s: Exp[Any], o: Exp[Any])(implicit ctx: SourceContext): Rep[String] = StringPlus(s,o)
  def string_startswith(s1: Exp[String], s2: Exp[String])(implicit ctx: SourceContext) = StringStartsWith(s1,s2)
  def string_trim(s: Exp[String])(implicit ctx: SourceContext) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String], limit : Exp[Int])(implicit ctx: SourceContext) : Rep[Array[String]] = StringSplit(s, separators, limit)
  def string_valueof(a: Exp[Any])(implicit ctx: SourceContext) = StringValueOf(a)
  def string_todouble(s: Exp[String])(implicit ctx: SourceContext) = StringToDouble(s)
  def string_replaceall(s : Exp[String], regex : Exp[String], repl : Exp[String])(implicit ctx: SourceContext) = StringReplaceAll(s, regex, repl)
  def string_contains(s : Exp[String], substring : Exp[String])(implicit ctx: SourceContext) = StringContains(s, substring)
  def string_isempty(s : Exp[String])(implicit ctx: SourceContext) = StringIsEmpty(s)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case StringPlus(a,b) => string_plus(f(a),f(b))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenStringOps extends ScalaGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringStartsWith(s1,s2) => emitValDef(sym, "%s.startsWith(%s)".format(quote(s1),quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep, limit) => emitValDef(sym, "%s.split(%s, %s)".format(quote(s), quote(sep), quote(limit)))
    case StringValueOf(a) => emitValDef(sym, "java.lang.String.valueOf(%s)".format(quote(a)))
    case StringToDouble(s) => emitValDef(sym, "%s.toDouble".format(quote(s)))
    case StringReplaceAll(s, regex, rep) => emitValDef(sym, "%s.replaceAll(%s, %s)".format(quote(s), quote(regex), quote(rep)))
    case StringContains(s, substring) => emitValDef(sym, "%s.contains(%s)".format(quote(s), quote(substring)))
    case StringIsEmpty(s) => emitValDef(sym, "%s.isEmpty()".format(quote(s)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenStringOps extends CudaGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringStartsWith(s1,s2) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringSplit(s, sep, limit) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringValueOf(a) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringToDouble(s) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringReplaceAll(s, regex, rep) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringContains(s, substring) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringIsEmpty(s) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenStringOps extends OpenCLGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringStartsWith(s1,s2) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringSplit(s, sep, limit) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringValueOf(a) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringToDouble(s) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringReplaceAll(s, regex, rep) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringContains(s, substring) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringIsEmpty(s) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CGenStringOps extends CGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym,"strcat(%s,%s);".format(quote(s1),quote(s2)))
    case StringStartsWith(s1,s2) => throw new GenerationFailedException("CGenStringOps: StringStartsWith not implemented yet")
    case StringTrim(s) => throw new GenerationFailedException("CGenStringOps: StringTrim not implemented yet")
    case StringSplit(s, sep, limit) => throw new GenerationFailedException("CGenStringOps: StringSplit not implemented yet")
    case StringValueOf(a) => throw new GenerationFailedException("CGenStringOps: StringValueOf not implemented yet")
    case StringToDouble(s) => throw new GenerationFailedException("CGenStringOps: StringToDouble not implemented yet")
    case StringReplaceAll(s, regex, rep) => throw new GenerationFailedException("CGenStringOps: StringReplaceAll not implemented yet")
    case StringContains(s, substring) => throw new GenerationFailedException("CGenStringOps: StringContains not implemented yet")
    case StringIsEmpty(s) => throw new GenerationFailedException("CGenStringOps: StringIsEmpty not implemented yet")
    case _ => super.emitNode(sym, rhs)
  }
}
