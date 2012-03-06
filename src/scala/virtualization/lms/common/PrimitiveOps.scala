package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.util.OverloadHack

trait LowPriorityPrimitiveImplicits {
  this: Variables with ImplicitOps =>

  implicit def intToRepDouble(i: Int): Rep[Double] = unit(i.toDouble)
  implicit def intToRepFloat(i: Int): Rep[Float] = unit(i.toFloat)
  implicit def floatToRepDouble(f: Float): Rep[Double] = unit(f.toDouble)

  implicit def repIntToRepDouble(x: Rep[Int]): Rep[Double] = implicit_convert[Int,Double](x)
  implicit def repIntToRepFloat(x: Rep[Int]): Rep[Float] = implicit_convert[Int,Float](x)
  implicit def repFloatToRepDouble(x: Rep[Float]): Rep[Double] = implicit_convert[Float,Double](x)
}

trait PrimitiveOps extends Variables with OverloadHack with LowPriorityPrimitiveImplicits {
  this: ImplicitOps =>

  /**
   * High priority lifting implicits
   */
  implicit def intToRepInt(x: Int): Rep[Int] = unit(x)
  implicit def floatToRepFloat(x: Float): Rep[Float] = unit(x)
  //implicit def doubleToRepDouble(x: Double): Rep[Double] = unit(x)


  /**
   *  Double
   */
  implicit def doubleToDoubleOps(n: Double) = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]) = new DoubleOpsCls(n)
  implicit def varDoubleToDoubleOps(n: Var[Double]) = new DoubleOpsCls(readVar(n))
  
  object Double {
    def parseDouble(s: Rep[String]) = obj_double_parse_double(s)
    def PositiveInfinity = obj_double_positive_infinity
    def MinValue = obj_double_min_value
  }

  class DoubleOpsCls(lhs: Rep[Double]){
    def floatValue() = double_float_value(lhs)
  }

  def obj_double_parse_double(s: Rep[String]): Rep[Double]
  def obj_double_positive_infinity: Rep[Double]
  def obj_double_min_value: Rep[Double]
  def double_float_value(lhs: Rep[Double]): Rep[Float]

  /**
   * Int
   */

  object Integer {
    def parseInt(s: Rep[String]) = obj_integer_parse_int(s)
  }

  object Int {
    def MaxValue = obj_int_max_value
  }

  implicit def intToIntOps(n: Int) = new IntOpsCls(n)
  implicit def repIntToIntOps(n: Rep[Int]) = new IntOpsCls(n)
  implicit def varIntToIntOps(n: Var[Int]) = new IntOpsCls(readVar(n))
    
  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash        
    //def /[A](rhs: Rep[A])(implicit mA: Manifest[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
    // TODO Something is wrong if we just use floatValue. implicits get confused
    def floatValueL() = int_float_value(lhs)
    def doubleValue() = int_double_value(lhs)
    def unary_~() = int_bitwise_not(lhs)
  }

  def infix_/(lhs: Rep[Int], rhs: Rep[Int]) = int_divide(lhs, rhs)
  def infix_%(lhs: Rep[Int], rhs: Rep[Int]) = int_mod(lhs, rhs)
  def infix_&(lhs: Rep[Int], rhs: Rep[Int]) = int_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Int], rhs: Rep[Int]) = int_binaryor(lhs, rhs)
  def infix_^(lhs: Rep[Int], rhs: Rep[Int]) = int_binaryxor(lhs, rhs)

  def obj_integer_parse_int(s: Rep[String]): Rep[Int]
  def obj_int_max_value: Rep[Int]
  def int_divide_frac[A:Manifest:Fractional](lhs: Rep[Int], rhs: Rep[A]): Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_mod(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_float_value(lhs: Rep[Int]): Rep[Float]
  def int_double_value(lhs: Rep[Int]): Rep[Double]
  def int_bitwise_not(lhs: Rep[Int]) : Rep[Int]
}

trait PrimitiveOpsExp extends PrimitiveOps with BaseExp {
  this: ImplicitOps =>

  /**
   * Double
   */
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]
  case class ObjDoublePositiveInfinity() extends Def[Double]
  case class ObjDoubleMinValue() extends Def[Double]
  case class DoubleFloatValue(lhs: Exp[Double]) extends Def[Float]

  def obj_double_parse_double(s: Exp[String]) = ObjDoubleParseDouble(s)
  def obj_double_positive_infinity = ObjDoublePositiveInfinity()
  def obj_double_min_value = ObjDoubleMinValue()
  def double_float_value(lhs: Exp[Double]) = DoubleFloatValue(lhs)

  /**
   * Int
   */
  case class ObjIntegerParseInt(s: Exp[String]) extends Def[Int]
  case class ObjIntMaxValue() extends Def[Int]
  case class IntDivideFrac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]
  case class IntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMod(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryOr(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryAnd(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryXor(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntDoubleValue(lhs: Exp[Int]) extends Def[Double]
  case class IntFloatValue(lhs: Exp[Int]) extends Def[Float]
  case class IntBitwiseNot(lhs: Exp[Int]) extends Def[Int]

  def obj_integer_parse_int(s: Rep[String]) = ObjIntegerParseInt(s)
  def obj_int_max_value = ObjIntMaxValue()
  def int_divide_frac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int]) : Exp[Int] = IntDivide(lhs, rhs)
  def int_mod(lhs: Exp[Int], rhs: Exp[Int]) = IntMod(lhs, rhs)
  def int_binaryor(lhs: Exp[Int], rhs: Exp[Int]) = IntBinaryOr(lhs, rhs)
  def int_binaryand(lhs: Exp[Int], rhs: Exp[Int]) = IntBinaryAnd(lhs, rhs)
  def int_binaryxor(lhs: Exp[Int], rhs: Exp[Int]) = IntBinaryXor(lhs, rhs)
  def int_double_value(lhs: Exp[Int]) = IntDoubleValue(lhs)
  def int_float_value(lhs: Exp[Int]) = IntFloatValue(lhs)
  def int_bitwise_not(lhs: Exp[Int]) = IntBitwiseNot(lhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = ({
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    e match {
      case IntDoubleValue(x) => int_double_value(f(x))
      case IntFloatValue(x) => int_float_value(f(x))
      case IntBitwiseNot(x) => int_bitwise_not(f(x))
      case IntDivide(x,y) => int_divide(f(x),f(y))
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "scala.Double.PositiveInfinity")
    case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
    case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
    case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case IntDoubleValue(lhs) => emitValDef(sym, quote(lhs) + ".doubleValue()")
    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenPrimitiveOps extends CLikeGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  //TODO: stdlib.h needs to be included in the common header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      //case ObjDoubleParseDouble(s) => emitValDef(sym, "atof(" + quote(s) + ")")
    	case ObjDoublePositiveInfinity() => emitValDef(sym, "DBL_MAX")
      //case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
      case DoubleFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
      //case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
      //case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
      case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
      case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
      case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
      case IntDoubleValue(lhs) => emitValDef(sym, "(double)"+quote(lhs))
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase with CLikeGenPrimitiveOps
trait OpenCLGenPrimitiveOps extends OpenCLGenBase with CLikeGenPrimitiveOps
trait CGenPrimitiveOps extends CGenBase with CLikeGenPrimitiveOps

