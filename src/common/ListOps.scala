package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen

trait ListOps extends Base {

  object List {
    def apply[A:Manifest](xs: Rep[A]*) = list_new(xs)
  }

  def list_new[A:Manifest](xs: Seq[Rep[A]]) : Rep[List[A]]
  def infix_++[A:Manifest](l1 : Rep[List[A]], l2: Rep[List[A]]) = list_++(l1, l2)
  def infix_size[A:Manifest](l1 : Rep[List[A]]) = list_size(l1)
  def list_size[A:Manifest](l1 : Rep[List[A]]) : Rep[Int]
  def list_++[A:Manifest](l1 : Rep[List[A]], l2: Rep[List[A]]) : Rep[List[A]]
}

trait ListOpsExp extends ListOps with EffectExp {
  case class ListNew[A:Manifest](xs: Seq[Rep[A]]) extends Def[List[A]]
  case class ListConcat[A:Manifest](l1: Rep[List[A]], l2: Rep[List[A]]) extends Def[List[A]]
  case class ListSize[A:Manifest](xs: Rep[List[A]]) extends Def[Int]

  def list_new[A:Manifest](xs: Seq[Rep[A]]) = ListNew(xs)
  def list_size[A:Manifest](l1 : Rep[List[A]]) = ListSize(l1)
  def list_++[A:Manifest](l1 : Rep[List[A]], l2: Rep[List[A]]) = ListConcat(l1, l2)
}

trait BaseGenListOps extends GenericNestedCodegen {
  val IR: ListOpsExp
  import IR._

}

trait ScalaGenListOps extends BaseGenListOps with ScalaGenEffect {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ListNew(xs) => emitValDef(sym, "List(" + (xs map {quote}).mkString(",") + ")")
    case ListConcat(l1, l2) => emitValDef(sym, "%s++%s".format(quote(l1), quote(l2)))
    case ListSize(l1) => emitValDef(sym, "%s.size".format(quote(l1)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenListOps extends BaseGenListOps with CLikeGenBase {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenListOps extends CudaGenEffect with CLikeGenListOps
trait OpenCLGenListOps extends OpenCLGenEffect with CLikeGenListOps
trait CGenListOps extends CGenEffect with CLikeGenListOps

