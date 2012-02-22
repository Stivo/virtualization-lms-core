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

trait HadoopGen extends ScalaGenBase with ScalaGenFunctions with ScalaGenUtil with ScalaGenVector {
	//val IR: dsl.type = dsl
	val IR: VectorOpsExp
	import IR._

	class MscrPart (var mscr : Mscr = null)

	case class Mapper(val input : Node) extends MscrPart {
		override def toString() = "M(%s)".format(input.id)
	}

	case class Reducer(val firstNode : Node) extends MscrPart {
		override def toString() = "R(%s)".format(firstNode.id)
	}
	
	case class GroupByKeyPart(val groupByKey : Node) extends MscrPart {
	    groupByKey.mscrPart += this
        override def toString() = "GBK(%s)".format(groupByKey.id)
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
		def name = toString//.takeWhile(_!='(')
		def isRead = false
		
		def isInGbkOrReducer = mscrPart.find(!_.isInstanceOf[Mapper]).isDefined
	}
	case class GroupByKey(override val id : Int) extends Node
	case class FlatMap(override val id : Int) extends Node
	case class Read(override val id : Int) extends Node {
		override def isRead = true
	}
	case class Save(override val id : Int) extends Node
	case class Combine(override val id : Int) extends Node
	case class Flatten(override val id : Int) extends Node
	case class Ignore(override val id : Int) extends Node
	def getNodes(graph : Graph[NodeType,EdgeType], mscrPart : MscrPart) =
		graph.nodes.map(_.value).filter(_.mscrPart.contains(mscrPart))
	
	def getPartnerNode(id : Int, x : Any) = x match {
		case NewVector(_) => Read(id)
		case VectorFlatten(_, _) => Flatten(id)
		case VectorMap(Sym(x), _) => FlatMap(id)
		case VectorFilter(Sym(x), _) => FlatMap(id)
		case VectorFlatMap(Sym(x), _) => FlatMap(id)
		case Reflect(VectorSave(Sym(x), _),_,_) => Save(id)
		case VectorGroupByKey(Sym(x)) => GroupByKey(id)
		case VectorReduce(Sym(x), f) => Combine(id)
		case Reify(Sym(x),_,_) => Save(id)
		case Reify(_,_,_) => Save(id)
		case _ => throw new RuntimeException("TODO: Add Partner Node for "+x)
	}
	
	def getInputs(x : Any) : List[Int] = x match {
		case VectorFlatten(Sym(x1), Sym(x2)) => List(x1, x2)
		case VectorMap(Sym(x), _) => List(x)
		case VectorFlatMap(Sym(x), _) => List(x)
		case VectorFilter(Sym(x), _) => List(x)
		case Reflect(VectorSave(Sym(x), _),_,_) => List(x)
		case Reify(Sym(x),_,_) => List(x)
		case Reify(_,_,_) => Nil
		case VectorGroupByKey(Sym(x)) => List(x)
		case VectorReduce(Sym(x), f) => List(x)
		case _ => Nil
	}
	
	def getName(x : Any) : String = x match {
		case Reflect(VectorSave(Sym(x), Const(name)),_,_) => "Save to %s".format(name)
		case VectorMap(vec, x) => "FlatMap (Map)"
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
	
	def getEdgeNotes(from : Node, to : Node) : String = {
	  
	  def getMscr(node : Node) = node.mscrPart.head.mscr
	  if (getMscr(from)!=getMscr(to)) {
	    return "Create intermediate store / read"
	  }
	  to match {
	    case GroupByKey(x) => 
	    	return if (getMscr(to).groups.size > 1) {
	    	  "emit key with tag "+getMscr(to).groups.indexWhere(_.groupByKey==to) 
	    	} else {
	    	  "emit"
	    	}
	    case _ => 
	  }
	  from match {
	    case GroupByKey(x) => 
	    	if (getMscr(from).groups.size > 1) {
	    	  return "use only keys with tag"+getMscr(from).groups.indexWhere(_.groupByKey==from) 
	    	} 
	    case _ => 
	  }
	  if (getMscr(from)!=getMscr(to)) {
	    return "Create intermediate store / read"
	  }
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
				thisNode : graph.EdgeT => thisNode.from.value match {
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
				()
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
				sw.write("""%s [label="%s%s"];
						""".format(node.id, node.name, inStr))
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
	
		override def toString() = "GraphState "
	}
	object GraphState extends ThreadLocal[GraphState]{
		override def get = {
		  var out = super.get
		  // ugly null test, some bug
		  if (!out.isInstanceOf[GraphState]) {
		    out = new GraphState()
		    super.set(out)
		  }
		  out
		}
	}
	
	def graphState = GraphState.get()
	
	override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
		GraphState.set(null)
		val out = super.emitSource(f, className, stream)
		FileOutput.writeln(graphState.export)
		out
	}
	
	override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
	
		val op = findDefinition(sym).get.rhs
		val partnerNode = {
			if (graphState.map.contains(sym.id))
				graphState.map(sym.id)
			else
				getPartnerNode(sym.id, op)
		}
		graphState.map(sym.id) = partnerNode
		val otherAttributes = getOtherAttributes(op).map( x=> "%s=%s".format(x._1, x._2)).mkString(",")
		for (x <- getInputs(rhs)) {
			val node = graphState.map(x)
					graphState.builder += node ~> partnerNode
		}
	
		super.emitNode(sym, rhs)
	}

}
