package scala.virtualization.lms
package epfl
package distributed

import common.ScalaGenBase
import common.ScalaGenFunctions
import scala.collection.mutable.Buffer
import scalax.collection.GraphEdge.DiEdge
import java.io.PrintWriter
import scalax.collection.immutable.Graph
import scala.collection.mutable.HashMap
import scalax.collection.GraphPredef.any2EdgeAssoc
import scalax.collection.edge.Implicits._
import scalax.collection.GraphEdge._
import java.io.StringWriter
import scalax.collection.GraphTraversal.VisitorReturn._
import scalax.collection.GraphTraversal._
import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.internal.Transforming
import internal.GenericFatCodegen
import common.SimplifyTransform
import internal.FatScheduling
import scala.collection.mutable


trait HadoopGen extends HadoopCodeGen with VectorBaseCodeGenPkg

trait HadoopCodeGen extends ScalaGenBase with ScalaGenVector {
	//val IR: dsl.type = dsl
    val IR: VectorOpsExp
	import IR.{Sym, Def, Exp, Reify, Reflect, Const}
	import IR.{NewVector, VectorSave, VectorMap, VectorFilter, VectorFlatMap, VectorFlatten, VectorGroupByKey, VectorReduce
	  , ComputationNode, VectorSaves}
	import IR.{TTP, TP, SubstTransformer, IRNode, Transformer}
	import IR.{findDefinition}
	import IR.{ClosureNode, freqHot, freqNormal, Lambda}
	
	class MscrPart (var mscr : Mscr = null)

	case class Mapper(val input : Node) extends MscrPart {
		override def toString() = "M_%s".format(input.id)
	}

	case class Reducer(val firstNode : Node) extends MscrPart {
		override def toString() = "R_%s".format(firstNode.id)
	}
	
	case class GroupByKeyPart(val groupByKey : Node) extends MscrPart {
	    groupByKey.mscrPart += this
        override def toString() = "GBK_%s".format(groupByKey.id)
	}
	
	case class Mscr(val mappers : List[Mapper], val reducers : List[Reducer], val groups : List[GroupByKeyPart]) {
		def all = mappers ++ reducers ++ groups
		all.foreach(_.mscr = this)
		override def toString() = "MSCR: %sM, %sGBK, %sR".format(mappers.size, groups.size, reducers.size)
	}
	
	type EdgeType[A] = DiEdge[A]
	type NodeType = Node

	trait Node {
		val id : Int
		var mscrPart = Buffer[MscrPart]()
		var graphOptions = ""
		def name = toString//.takeWhile(_!='(')
		def isRead = false
		def mscr = mscrPart.head.mscr
		def isInGbkOrReducer = mscrPart.find(!_.isInstanceOf[Mapper]).isDefined
		def getOriginal = Sym(id) match {
		  case Def(d) => d
		  case _ => throw new RuntimeException("Did not find symbol "+id)
		}
		def getTypes = getOriginal match {
		  case x : ComputationNode => x.getTypes
		  case Reflect(x:ComputationNode,_,_) => x.getTypes
		  case _ => (manifest[Nothing],manifest[Nothing])
		}
	}
	case class GroupByKey(override val id : Int) extends Node 
	case class FlatMap(override val id : Int) extends Node
	case class Read(override val id : Int) extends Node {
		override def isRead = true
	}
	case class Save(override val id : Int) extends Node
	case class AllSave(override val id : Int) extends Node
	case class Reduce(override val id : Int) extends Node
	case class Flatten(override val id : Int) extends Node
	case class Ignore(override val id : Int) extends Node
	def getNodes(graph : Graph[NodeType,EdgeType], mscrPart : MscrPart) =
		graph.nodes.map(_.value).filter(_.mscrPart.contains(mscrPart))
	
	def getPartnerNode(id : Int, x : Any) = {
	  val out = x match {
			case NewVector(_) => Read(id)
			case VectorFlatten(_, _) => Flatten(id)
			case VectorMap(Sym(x), _) => FlatMap(id)
			case VectorFilter(Sym(x), _) => FlatMap(id)
			case VectorFlatMap(Sym(x), _) => FlatMap(id)
			case Reflect(VectorSave(Sym(x), _),_,_) => Save(id)
			case VectorSave(Sym(x), _) => Save(id)
			case VectorGroupByKey(Sym(x)) => GroupByKey(id)
			case VectorReduce(Sym(x), f) => Reduce(id)
			case Reify(Sym(x),_,_) => Save(id)
			case Reify(_,_,_) => Save(id)
//			case VectorSaves(_) => AllSave(id)
//			case _ => {println("did not find for "+id+ " "+x); null}
			case _ => null
//			case _ => throw new RuntimeException("TODO: Add Partner Node for "+x)
		}
	  if (out != null)
	    Some(out)
	  else
	    None
	}
	
	def getInputs(x : Any) : List[Int] = 
	  x match {
		case VectorFlatten(Sym(x1), Sym(x2)) => List(x1, x2)
		case VectorMap(Sym(x), _) => List(x)
		case VectorFlatMap(Sym(x), _) => List(x)
		case VectorFilter(Sym(x), _) => List(x)
		case Reflect(VectorSave(Sym(x), _),_,_) => List(x)
		case VectorSave(Sym(x), _) => List(x)
		case Reify(Sym(x),_,_) => List(x)
		case Reify(_,_,_) => Nil
		case VectorGroupByKey(Sym(x)) => List(x)
		case VectorReduce(Sym(x), f) => List(x)
		case vs@VectorSaves(saves) => vs.ids
		case _ => Nil
	}
	
	def getName(x : Any) : String = x match {
		case Reflect(VectorSave(Sym(x), Const(name)),_,_) => "Save to %s".format(name)
		case VectorMap(vec, x) => "FlatMap (Map)"
		case VectorFilter(vec, x) => "FlatMap (Filter)"
		case VectorFlatMap(vec, x) => "FlatMap"
		case NewVector(Const(name)) => "Read from %s".format(name)
		case VectorFlatten(v1, v2) => "Flatten"
		case VectorGroupByKey(Sym(x)) => "Group By Key"
		case VectorReduce(Sym(x), f) => "Reduce"
		//    case x : Node => x.toString.
		//    case VectorMap(Sym(x), _) => List(x)
		case Reify(_,_,_) => "Reify"
		case _ => "Unnamed"
	}
	
	def getOtherAttributes(x : Any) : List[(String, String)] = x match {
		case VectorGroupByKey(_) => List(("shape", "box"), ("color","red"))
		case VectorFlatten(_, _) => List(("shape", "triangle"))
		case NewVector(_) => List(("color","green"))
		case Reflect(VectorSave(_,_),_,_) => List(("color","blue"))
		case _ => Nil
	}

class GraphState {
	var map = HashMap[Int, NodeType]()
	val builder = Graph.newBuilder[NodeType, EdgeType]
	def graph : Graph[NodeType, EdgeType]= builder.result()
	def export : String = exportMSCRGraph(graph) 

	def export(graph : Graph[NodeType, EdgeType]) : String = {
		val sw = new StringWriter()
		sw.write("digraph {\n")
		for (node <- graph.nodes) {
			sw.write("""%s [label="%s in %s"];
					""".format(node.id, node.name, node.mscrPart.mkString(",")))
		}
		for (edge <- graph.edges) {
			sw.write("%s -> %s;\n".format(edge.from.id, edge.to.id))
		}
		sw.write("}")
		sw.toString()
	}

	def getNodeNotes(node : Node) : String = {
	  object StartOfMapper {
	    def unapply(node : Node) = if (node.mscrPart.size==1) {
	     node.mscrPart.head match {
	       case m@Mapper(node) => Some(m)
	       case _ => None
	     }
	    } else {
	       None
	    }
	  }
  	    node match {
	      case StartOfMapper(x) if x.input == node => return "read from mapper input and convert"
	      case _ => 
	    }
	  ""
	}
	
	object TaggedGroupByKey {
	  def unapply(node : Node) = 
	  node match {
	    case GroupByKey(id) if (node.mscr.groups.size > 1) => 
	      	Some((node,node.mscr.groups.indexWhere(_.groupByKey==node)))
	    case _ => None
	  }
	}
	
//	object SpecialEdge {
//	  def unapply(edge : DiEdge[Node]) = edge match {
//	    case TaggedKeyEdge(edge) => Some(edge)
//	    case _ => None
//	  }
//	}
//	object TaggedKeyEdge {
//	  def unapply(edge : graph.EdgeT) = edge match{
//	    case TaggedGroupByKey(edge.to, tag)  => Some(edge)
//	    case TaggedGroupByKey(edge.from, tag)  => Some(edge)
//	    case _ => None
//	  }
//	}
	
	def getEdgeNotes(from : Node, to : Node) : String = {
	  
	  def getMscr(node : Node) = node.mscrPart.head.mscr
//	  if (getMscr(from)!=getMscr(to)) {
//	    return "Create intermediate store / read"
//	  }
	  to match {
	    case TaggedGroupByKey(x, tag) => return "emit key with tag "+tag 
	    case GroupByKey(x) => return "emit"
	    case _ => 
	  }
	  from match {
	    case TaggedGroupByKey(x, tag) => return "use only keys with tag "+tag 
	    case _ => 
	  }
//	  if (getMscr(from)!=getMscr(to)) {
//	    return "Create intermediate store / read"
//	  }
	  ""
	}
	
	def createMSCRs(graph : Graph[NodeType, EdgeType]) : Iterable[Mscr] = {

		val inputNodes = graph.nodes.filter(_.isRead).map(_.value)
		var toDo = Buffer[Node]()
		toDo ++= inputNodes
		var out = Buffer[Mscr]()
		def findAllRelated(node : Node) = {
			var starts = Set[Node](node)
			var gbks = Set[Node]()

			def makeFilter(node : Node) = {
				thisNode : graph.NodeT => 
			!(node != thisNode.value && 
			(starts.contains(thisNode.value) || gbks.contains(thisNode.value)))
				}

			def makeEdgeFilter(node : graph.NodeT) = {
				thisNode : graph.EdgeT =>
				  thisNode.from.value match {
					case x : Any if x == node.value => true
					case GroupByKey(_) => false
					case x if x.isInGbkOrReducer => false 
					case _ => true
				}
			}

			def goBackward(node : graph.NodeT) {
				node.traverseNodes(direction = Predecessors, edgeFilter = makeEdgeFilter(node)
						, nodeFilter = makeFilter(node.value)){
					x =>
//					println("backward "+x.value)
					x.value match {
					  // not if there is a non mapper mscrpart in the mscrPart (this is already assigned)
					  case node : Node if node.mscrPart.find(!_.isInstanceOf[Mapper]).isDefined
					  	=> starts += node; goForward(x); Continue
					case Read(_) => starts += x.value; goForward(x); Continue
					case _ => Continue
					}
				}
			}

			def goForward(node : graph.NodeT) {
				node.traverseNodes(edgeFilter = _.from.value match {case GroupByKey(_) => false; case _ => true}){
					x=>
//					println("forward "+x.value)
					x.value match {
					  case _ if gbks.contains(x.value) => Continue
					case gbk@GroupByKey(_) => gbks += x.value; goBackward(x); Continue
					case _ => Continue
					}
				}
			}
			goForward(graph.get(node))
			starts
		}

			def makeMapper(node : graph.NodeT) = {
				var nodes = Set[Node]()
				var gbks = Set[Node]() 
				node.traverseNodes(edgeFilter = _.from.value match {case GroupByKey(_) => false; case _ => true}){
					x=>
					x.value match {
						case GroupByKey(_) => gbks += x.value; Continue
						case _ => nodes += x.value; Continue
					}
				}
				val mapper = new Mapper(node.value)
				nodes.foreach(_.mscrPart += mapper)
				(gbks, mapper)
			}
			while (!toDo.isEmpty)
			{
				val head = toDo.head
				// make an mscr
				// find all related starts for this mscr
				val starts = findAllRelated(head)
				toDo --= starts
				// make the mappers
				val mappersAndGBKs = starts.map(x => makeMapper(graph.get(x)))
				val mappers = mappersAndGBKs.map(_._2)
				// make the found group by keys to group msrc parts
				val gbks = mappersAndGBKs.map(_._1).reduce(_++_)
				val groups = gbks.map{ x => new GroupByKeyPart(x)}
				
				// make the reducers, one per group by key
				// the filter, that stops traversing when a new MSCR should be started
				def reducerEdgeFilter(edge : graph.EdgeT) : Boolean = {
				  def cutOnThisEdge : Boolean = {
					  edge.to.value match {
					    case GroupByKey(_) => return true
					    case _ =>
					  }
					  val successorsWithGBKs = edge.to.diSuccessors.filter({
					    x =>
					      x.value match {
					        case GroupByKey(_) => true
					        case _ => x.findSuccessor(_.value match {case GroupByKey(_) => true; case _ => false}).isDefined
					      }
					  })
					  // TODO: only count branches which lead to group by keys, as others can be directly implemented
					  if (successorsWithGBKs.size > 1){
					    return true
					  }
					  return false
				  }
				  if (cutOnThisEdge) {
				    toDo += edge.to.value;
				    false
				  } else {
				    true
				  }
				}
				// make the reducers
				val reducers = gbks.map{
					x =>
	
					var reducer = graph.get(x).diSuccessors.head.value
					var nodes = Set[Node]()
					graph.get(x).traverseNodes(edgeFilter = reducerEdgeFilter) {
						x=>
						nodes += x;
						Continue
					}
					nodes -= x
					val red = new Reducer(reducer)
					nodes.foreach(_.mscrPart += red)
					red
				}
				out += new Mscr(mappers.toList, reducers.toList, groups.toList)
			}
			out
		}

		def exportMSCRGraph(graph : Graph[NodeType, EdgeType]) = {
			val mscrs = createMSCRs(graph)
					val sw = new StringWriter()
			sw.write("digraph {\n")
			var i = 0
			for (node <- graph.nodes) {
			  val inStr = if (node.mscrPart.size > 1) 
				  			" in %s".format(node.mscrPart.mkString(","))
				  		  else
				  		    ""
				sw.write("""%s [label="%s%s",%s];
""".format(node.id, node.name, inStr, node.graphOptions))
			}
			for (mscr <- mscrs) {
				sw.write("subgraph cluster%s {\n".format(i))
				i+=1
				sw.write("label = \"%s\";\n".format(mscr))
				for (part <- mscr.all) {
					sw.write("subgraph cluster%s {\n".format(i))
					i +=1
					sw.write("label = \"%s\";\n".format(part.toString))
					for (node <- getNodes(graph, part)) {
						sw.write(node.id+";\n")
					}
					sw.write("}\n")
				}
				sw.write("}\n")
			}
			for (edge <- graph.edges) {
			  val notes = getEdgeNotes(edge.from.value, edge.to.value)
				sw.write("%s -> %s [label=\"%s\"];\n".format(edge.from.id, edge.to.id, notes))
			}
			sw.write("}")
			sw.toString()
		}
		
		def writeTypes {
		  val nodes = graph.nodes.map{x : { def value : Node } => x.value}.toList
		  System.out.println("---------- TYPES --------------")
		  for (node <- nodes.sortBy(_.id)) {
		    System.out.println(node.getOriginal)
		    node.getOriginal match {
		      case x : ComputationNode => println(x.getTypes)
		      case _ =>
		    }
//		    System.out.println(node.getOriginal.getTypes)
		  }
		}
	
	
		override def toString() = "GraphState "
	}
	object GraphState extends ThreadLocal[GraphState]{
		override def get = {
		  var out = super.get
		  if (out == null) {
		    out = new GraphState()
		    super.set(out)
		  }
		  out
		}
	}
	
	def graphState = GraphState.get()
	
	// TODO work in progress
	trait Transformation {
	  this : { 
	    def addSubstitution(sym1 : Exp[_], sym2 : Exp[_])
//	    def makeTTP() 
	  } =>
	  def appliesToNode(inExp : Exp[_]) : Boolean
	  def applyToNode(inExp : Exp[_], context : Any) {
		  val out = doTransformation(inExp);
		  // get dependencies
		  // find all new Defs
		  // make TTP's from defs
		  // add the TTPs to the scope
	  }
	  def doTransformation(inExp : Exp[_]) : Def[_]
			  
	}
	
  override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: List[Exp[Any]])(body: List[TTP] => A): A = {
    var result1 = result0
    var scope1 = currentScope0
    var transformer = new SubstTransformer
    val toDo = mutable.HashSet[Exp[_]]()
      def addSubstitution(sym1 : Exp[_], sym2 : Exp[_]) {
        System.err.println("Adding substitution "+sym1+" = "+sym2)
        transformer.subst(sym1) = sym2
      }
      class MarkerTransformer extends SubstTransformer {
      
        override def apply[A](inExp: Exp[A]) : Exp[A] = {
          val symdeps = inExp match {
            case Def(x) => IR.syms(x).mkString(", ")
            case _ => ""
          }
//          System.err.print("visiting "+inExp+" "+" symdeps ="+symdeps+"; ");
//          System.err.println(inExp match {
//            case Def(y) => y
//          	case _=> "not a def"
//          })
          var replace : Exp[_]= null
          inExp match {
            case Def(vm@VectorMap(Def(vf@VectorFlatten(v1, v2)),func)) => {
              System.err.println("Marking flatten "+vf)
              toDo+=inExp
            }
//            case Def(vm@VectorSave(Def(vf@VectorFlatten(v1, v2)),path)) => {
//              System.err.println("Sinking flatten behind save "+v1+" "+v2)
//          }
            case _ => 
          }

////          	case Def(v1@VectorMap(Def(v2@VectorMap(in, f1)), f2)) 
////          	if v2.reads <= 1 => {
////          	  replace = IR.syms(v1).apply(1)
////          	  def getInputSymbol(v : VectorMap[_,_]) = {
////	          	  val clos = v.closure match {
////	          	    case Def(l:Lambda[_,_]) => l
////	          	    case _ => null
////	          	  }
////	          	  clos.x
////          	  }
////          	  v1.alive = false
////          	  v2.alive = false
////          	  System.err.println("Merging VectorMaps "+v1+" "+v2)
////          	  val composed = VectorMap(in, f1.andThen(f2))(v2.mA, v1.mB)
////          	  
////          	  val out = IR.toAtom2(composed)
////          	  out match {
////          	    case Def(v : VectorMap[_,_]) => addSubstitution(getInputSymbol(v2),getInputSymbol(v))
////          	    case _ => 
////          	  }
////          	  out
////          	}
//          	case _ => inExp
//          }).asInstanceOf[Exp[A]]
//          if (out != inExp) {
//            addSubstitution(inExp, out)
//            out match {
//              case Def(vm@VectorMap(x,y)) => {
//                addSubstitution(replace, vm.closure)
//              }
//              case _ =>
//            }
//            
//          }
          inExp
        }
      }
      var i = 0
      do {
    	  toDo.clear
		  var marker = new MarkerTransformer()
    	  transformer = new SubstTransformer()
		  // with fatschedule: deps(IR.syms(result)).map(_.rhs)
		  var returned = transformAllFully(scope1, result1, marker);
		  scope1 = returned._1
		  result1 = returned._2
//		  scope1.foreach(println)
//		  result.foreach(println)
		  for (exp <- toDo) {
	          exp match {
	            case Def(vm@VectorMap(Def(vf@VectorFlatten(v1, v2)),func)) => {
	              System.err.println("found flatten "+vf)
	              val mapLeft = new VectorMap(v1,func)
	              val mapRight = new VectorMap(v2,func)
	              val defLeft = IR.toAtom2(mapLeft)
	              val defRight = IR.toAtom2(mapRight)
	              val flatten = new VectorFlatten(defLeft, defRight)
	              val flattened = IR.toAtom2(flatten)
	              addSubstitution(exp, flattened)
	              val newDefs = List(flatten, mapLeft, mapRight)
	              val ttps = newDefs.map(IR.findOrCreateDefinition(_,Nil)).map(fatten)
	              scope1 = scope1 ++ ttps
	              flattened
	            }
	//            case Def(vm@VectorSave(Def(vf@VectorFlatten(v1, v2)),path)) => {
	//              System.err.println("Sinking flatten behind save "+v1+" "+v2)
	          }
		    
		  }
	//	  t = new Transformer()
		  returned = transformAllFully(scope1, result1, transformer)
		  scope1 = returned._1
		  result1 = returned._2
		  i += 1
      } while (!toDo.isEmpty && i < 100)
	  buildGraph(scope1.flatMap{
	    x =>
	      x match {
	      	case TTP(_,IR.ThinDef(x)) => Some(x)
	      	case _ => None
	      }
	  }.map(IR.findOrCreateDefinition(_,Nil)))
	  super.focusExactScopeFat(scope1)(result1)(body)
	}
//	override def emitFatBlockFocused(scope: List[TTP])(result: List[Exp[Any]])(implicit stream: PrintWriter): Unit = {
//
//	  println("emitFatBlockFocused "+scope+" result "+result)
//	  super.getFatSchedule(transformed._1)(transformed._2)
//	}
//	
//    override def focusExactScopeFat[A](currentScope0: List[TTP])(result0: List[Exp[Any]])(body: List[TTP] => A): A = {
//	  super.focusExactScopeFat[A](transformed._1)(transformed._2)(body)
//	}	
	
	override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
		val out = super.emitSource(f, className, stream)
	  val defs = IR.globalDefs
	  val leftDefs = defs.flatMap{x => x.rhs match {case r@Reflect(vs@VectorSave(_,_),_,_) => Some((x.sym,vs)) case _ => None}}
      val savesSym = IR.fresh[VectorSaves](Nil)
      FileOutput.writeln(graphState.export)
	  graphState.writeTypes
	  out
	}
	
	def buildGraph(defs : List[IR.TP[_]]){
	  for (tp <- defs) {
	    val doThis = tp.rhs match {
	      case x : IRNode => x.alive
	      case Reflect(x : IRNode,_,_) => x.alive
	      case _ => true
	    }
	    if (doThis)
	    updateGraph(tp.sym, tp.rhs)
	  }
	}
	
	def updateGraph(sym: Sym[Any], rhs: Def[Any]): Unit = {
		val op = findDefinition(sym).get.rhs;
//		println(sym +" "+rhs)
		graphState.map.get(sym.id).orElse(getPartnerNode(sym.id, op))
		.map{ partnerNode =>
		   
			graphState.map(sym.id) = partnerNode
			val otherAttributes = getOtherAttributes(op).map( x=> "%s=%s".format(x._1, x._2)).mkString(",")
			partnerNode.graphOptions = otherAttributes
			for (x <- getInputs(rhs)) {
			  if (graphState.map.contains(x)) {
				val node = graphState.map(x)
				graphState.builder += node ~> partnerNode
			  }
			}
		}
	}

	override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
		super.emitNode(sym, rhs)
	}

}


