package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import scala.annotation.unchecked.uncheckedVariance
import java.lang.{StackTraceElement,Thread}

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def tp: Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
    def pos: List[SourceContext] = Nil
  }

  /**
   * Super trait for all statements that emit results from the loop. For example: Yield and Skip.
   */
  abstract class Gen[+T: Manifest]

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceInfo = Thread.currentThread.getStackTrace // will go away
    var sourceContexts: List[SourceContext] = Nil
    override def pos = sourceContexts
    def withPos(pos: List[SourceContext]) = { sourceContexts :::= pos; this }
  }

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  var nVars = 0
  def fresh[T:Manifest]: Sym[T] = Sym[T] { nVars += 1; nVars -1 }

  def fresh[T:Manifest](pos: List[SourceContext]): Sym[T] = fresh[T].withPos(pos)

  def quotePos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs => 
      def all(cs: SourceContext): List[SourceContext] = cs.parent match {
        case None => List(cs)
        case Some(p) => cs::all(p)
      }
    cs.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
  }

/*
  def fresh[T:Manifest] = {
    val (name, id, nameId) = nextName("x")
    val sym = Sym[T](id)
    sym.name = name
    sym.nameId = nameId
    sym
  }

  def fresh[T:Manifest](d: Def[T], ctx: Option[SourceContext]) = {
    def enclosingNamedContext(sc: SourceContext): Option[SourceContext] = sc.bindings match {
      case (null, _) :: _ =>
        if (!sc.parent.isEmpty) enclosingNamedContext(sc.parent.get)
        else None
      case (name, line) :: _ =>
        Some(sc)
    }

    // create base name from source context
    val (basename, line, srcCtx) = if (!ctx.isEmpty) {
      enclosingNamedContext(ctx.get) match {
        case None =>
          // no enclosing context has variable assignment
          var outermost = ctx.get
          while (!outermost.parent.isEmpty) {
            outermost = outermost.parent.get
          }
          ("x", 0, Some(outermost))
        case Some(sc) => sc.bindings match {
          case (n, l) :: _ =>
            (n, l, Some(sc))
        }
      }
    } else ("x", 0, None)
    val (name, id, nameId) = nextName(basename)
    val sym = Sym[T](id)
    sym.name = name
    sym.nameId = nameId
    sym.sourceContext = srcCtx
    sym
  }
*/

  abstract class Def[+T] // operations (composite)

  abstract class Stm { // statement (links syms and definitions)
     def syms : List[Sym[Any]]
     def defs : List[Def[Any]]
  }
  
  def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TP(sym, rhs) => List(sym)
  }
  
  def infix_rhs(stm: Stm): Any = stm match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
    case TP(sym, rhs) => rhs
  }

  def infix_defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TP(`sym`, rhs: Def[A]) => Some(rhs)
    case _ => None
  }

  def infix_defines[A](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TP(sym: Sym[A], `rhs`) => Some(sym)
    case _ => None
  }
  
  case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm {
    def syms = List(sym)
    def defs = List(rhs)
  }

  // graph construction state
  
  var globalDefs: List[Stm] = Nil
  var localDefs: List[Stm] = Nil

  def reifySubGraph[T](b: =>T): (T, List[Stm]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    localDefs = Nil
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    (r, defs)
  }

  def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    val existing = globalDefs filter (_.lhs exists (lhs contains _))
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
  }

  def findDefinition[T](s: Sym[T]): Option[Stm] =
    globalDefs.find(x => x.defines(s).nonEmpty)

  def findDefinition[T](d: Def[T]): Option[Stm] =
    globalDefs.find(x => x.defines(d).nonEmpty)

  def findOrCreateDefinition[T:Manifest](d: Def[T], pos: List[SourceContext]): Stm =
    findDefinition[T](d) map { x => x.defines(d).foreach(_.withPos(pos)); x } getOrElse {
      createDefinition(fresh[T](pos), d)
    }

  def findOrCreateDefinitionExp[T:Manifest](d: Def[T], pos: List[SourceContext]): Exp[T] =
    findOrCreateDefinition(d, pos).defines(d).get

  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    val f = TP(s, d)
    reflectSubGraph(List(f))
    f
  }
  

  protected implicit def toAtom[T:Manifest](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
    findOrCreateDefinitionExp(d, List(pos)) // TBD: return Const(()) if type is Unit??
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).flatMap(_.defines(s))
      case _ =>
        None
    }
  }


  // dependencies

  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case ss: Seq[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Seq[Any] => ss.toList.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }

  def effectSyms(x: Any): List[Sym[Any]] = x match {
    case ss: Seq[Any] => ss.toList.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }

  def softSyms(e: Any): List[Sym[Any]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(softSyms(_))
    case _ => Nil
  }


  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case ss: Seq[Any] => ss.toList.flatMap(f)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case ss: Seq[Any] => ss.toList.flatMap(symsFreq(_))
//    case _ => rsyms(e)(symsFreq)
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))



/*
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }
*/

/*
  def symsShare(e: Any): List[(Sym[Any], Int)] = {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(symsShare(_))
    case _ => Nil
  }
*/



  // bookkeeping

  def reset { // used by delite?
    nVars = 0
    globalDefs = Nil
  }

}
