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
  
  def infix_+(s1: String, s2: Rep[Any])(implicit o: Overloaded1, pos: SourceContext) = string_plus(unit(s1), s2)
  def infix_+[T:Manifest](s1: String, s2: Var[T])(implicit o: Overloaded2, pos: SourceContext) = string_plus(unit(s1), readVar(s2))
  def infix_+(s1: Rep[String], s2: Rep[Any])(implicit o: Overloaded1, pos: SourceContext) = string_plus(s1, s2)
  def infix_+[T:Manifest](s1: Rep[String], s2: Var[T])(implicit o: Overloaded2, pos: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[String], s2: Rep[String])(implicit o: Overloaded3, pos: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[String], s2: Var[String])(implicit o: Overloaded4, pos: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[Any], s2: Rep[String])(implicit o: Overloaded5, pos: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[Any], s2: Var[String])(implicit o: Overloaded6, pos: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[Any], s2: String)(implicit o: Overloaded7, pos: SourceContext) = string_plus(s1, unit(s2))
  
  def infix_+(s1: Var[String], s2: Rep[Any])(implicit o: Overloaded8, pos: SourceContext) = string_plus(readVar(s1), s2)  
  def infix_+[T:Manifest](s1: Var[String], s2: Var[T])(implicit o: Overloaded9, pos: SourceContext) = string_plus(readVar(s1), readVar(s2))
  def infix_+(s1: Var[String], s2: Rep[String])(implicit o: Overloaded10, pos: SourceContext) = string_plus(readVar(s1), s2)    
  def infix_+(s1: Var[String], s2: Var[String])(implicit o: Overloaded11, pos: SourceContext) = string_plus(readVar(s1), readVar(s2))    
  def infix_+[T:Manifest](s1: Var[T], s2: Rep[String])(implicit o: Overloaded12, pos: SourceContext) = string_plus(readVar(s1), s2)
  def infix_+[T:Manifest](s1: Var[T], s2: Var[String])(implicit o: Overloaded13, pos: SourceContext) = string_plus(readVar(s1), readVar(s2))
  def infix_+[T:Manifest](s1: Var[T], s2: String)(implicit o: Overloaded14, pos: SourceContext) = string_plus(readVar(s1), unit(s2))
  
  // these are necessary to be more specific than arithmetic/numeric +. is there a more generic form of this that will work?
  //def infix_+[R:Manifest](s1: Rep[String], s2: R)(implicit c: R => Rep[Any], o: Overloaded15, pos: SourceContext) = string_plus(s1, c(s2))  
  def infix_+(s1: Rep[String], s2: Double)(implicit o: Overloaded15, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Float)(implicit o: Overloaded16, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Int)(implicit o: Overloaded17, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Long)(implicit o: Overloaded18, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Short)(implicit o: Overloaded19, pos: SourceContext) = string_plus(s1, unit(s2))  
  
  def infix_trim(s: Rep[String]) = string_trim(s)
  def infix_split(s: Rep[String], separators: Rep[String])(implicit o : Overloaded1) : Rep[Array[String]] = string_split(s, separators, unit(0))
  def infix_split(s: Rep[String], separators: Rep[String], limit : Rep[Int])(implicit o : Overloaded2) : Rep[Array[String]] = string_split(s, separators, limit)
  def infix_startsWith(s1: Rep[String], s2: Rep[String]) = string_startswith(s1,s2)
  def infix_endsWith(s: Rep[String], e: Rep[String]) = string_endsWith(s,e)
  def infix_contains(s1: Rep[String], s2: Rep[String]) = string_contains(s1,s2)
  def infix_isEmpty(s1: Rep[String]) = string_isEmpty(s1)
  def infix_length(s1: Rep[String]) = string_length(s1)
  def infix_matches(s: Rep[String], regex: Rep[String]) = string_matches(s, regex)
  
  object String {
    def valueOf(a: Rep[Any])(implicit pos: SourceContext) = string_valueof(a)
  }

  def string_plus(s: Rep[Any], o: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def string_trim(s: Rep[String]): Rep[String]
  def string_split(s: Rep[String], separators: Rep[String], limit : Rep[Int]): Rep[Array[String]]
  def string_valueof(d: Rep[Any]): Rep[String]
  def string_startswith(s1: Rep[String], s2: Rep[String]): Rep[Boolean]
  def string_endsWith(s: Rep[String], e: Rep[String]): Rep[Boolean]
  def string_contains(s1: Rep[String], s2: Rep[String]) : Rep[Boolean]
  def string_isEmpty(s1: Rep[String]) : Rep[Boolean]
  def string_length(s1: Rep[String]) : Rep[Int]
  def string_matches(s: Rep[String], regex: Rep[String]): Rep[Boolean]
}

trait StringOpsExp extends StringOps with VariablesExp {
  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String], limit : Exp[Int]) extends Def[Array[String]]
  case class StringValueOf(a: Exp[Any]) extends Def[String]
  case class StringStartsWith(s1: Exp[String], s2: Exp[String]) extends Def[Boolean]
  case class StringEndsWith(s: Exp[String], e: Exp[String]) extends Def[Boolean]
  case class StringContains(s: Exp[String], sub: Exp[String]) extends Def[Boolean]
  case class StringIsEmpty(s: Exp[String]) extends Def[Boolean]
  case class StringLength(s: Exp[String]) extends Def[Int]
  case class StringMatches(string: Exp[String], pattern: Exp[String]) extends Def[Boolean]
  
  def string_plus(s: Exp[Any], o: Exp[Any])(implicit pos: SourceContext): Rep[String] = StringPlus(s,o)
  def string_trim(s: Exp[String]) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String], limit : Exp[Int]) : Rep[Array[String]] = StringSplit(s, separators, limit)
  def string_valueof(a: Exp[Any]) = StringValueOf(a)
  def string_startswith(s1: Exp[String], s2: Exp[String]) = StringStartsWith(s1,s2)
  def string_endsWith(s: Exp[String], e: Exp[String]) = StringEndsWith(s,e)
  def string_contains(s1: Exp[String], s2: Exp[String]) = StringContains(s1,s2)
  def string_isEmpty(s1: Exp[String]) = StringIsEmpty(s1)
  def string_length(s1: Exp[String]) = StringLength(s1)
  def string_matches(s: Rep[String], regex: Rep[String]) = StringMatches(s, regex)
  
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext) : Def[A] = (e match {
    case StringPlus(a,b) => StringPlus(f(a),f(b))
    case StringTrim(s) => StringTrim(f(s))
    case StringSplit(s, sep, l) => StringSplit(f(s), f(sep), f(l))
    case StringValueOf(a) => StringValueOf(a)
    case StringStartsWith(s, e) => StringStartsWith(f(s),f(e))
    case StringEndsWith(s, e) => StringEndsWith(f(s),f(e))
    case StringContains(s, sub) => StringContains(f(s),f(sub))
    case StringIsEmpty(s) => StringIsEmpty(f(s))
    case StringLength(s) => StringLength(f(s))
    case StringMatches(s, pat) => StringMatches(f(s), f(pat))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]
  
}

trait ScalaGenStringOps extends ScalaGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep, limit) => emitValDef(sym, "%s.split(%s, %s)".format(quote(s), quote(sep), quote(limit)))
    case StringValueOf(a) => emitValDef(sym, "java.lang.String.valueOf(%s)".format(quote(a)))
    case StringStartsWith(s1,s2) => emitValDef(sym, "%s.startsWith(%s)".format(quote(s1),quote(s2)))
    case StringEndsWith(s, e) => emitValDef(sym, "%s.endsWith(%s)".format(quote(s), quote(e)))
    case StringContains(s, e) => emitValDef(sym, "%s.contains(%s)".format(quote(s), quote(e)))
    case StringIsEmpty(s) => emitValDef(sym, "%s.isEmpty()".format(quote(s)))
    case StringLength(s) => emitValDef(sym, "%s.length".format(quote(s)))
    case StringMatches(s, pattern) => emitValDef(sym, "%s.matches(%s)".format(quote(s), quote(pattern)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenStringOps extends CudaGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringSplit(s, sep, lim) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringStartsWith(s, con) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenStringOps extends OpenCLGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringSplit(s, sep, lim) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringStartsWith(s, con) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CGenStringOps extends CGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym,"strcat(%s,%s);".format(quote(s1),quote(s2)))
    case StringTrim(s) => throw new GenerationFailedException("CGenStringOps: StringTrim not implemented yet")
    case StringSplit(s, sep, lim) => throw new GenerationFailedException("CGenStringOps: StringSplit not implemented yet")
    case StringStartsWith(s, con) => throw new GenerationFailedException("CGenStringOps: StringStartsWith not implemented yet")
    case _ => super.emitNode(sym, rhs)
  }
}
