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

	class Mapper(val input : Node) extends MscrPart {
		override def toString() = "M(%s)".format(input.id)
	}

	class Reducer(val firstNode : Node) extends MscrPart {
		override def toString() = "R(%s)".format(firstNode.id)
	}
	
	class GroupByKeyPart(val groupByKey : Node) extends MscrPart {
	  groupByKey.mscrPart += this
		override def toString() = "GBK(%s)".format(groupByKey.id)
	} 
	
	class Mscr(val mappers : Buffer[Mapper], val reducers : Buffer[Reducer], val groups : Buffer[GroupByKeyPart]) {
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
		case VectorFlatMap(Sym(x), _) => FlatMap(id)
		case Reflect(VectorSave(Sym(x), _),_,_) => Save(id)
		case VectorGroupByKey(Sym(x)) => GroupByKey(id)
		case VectorReduce(Sym(x), f) => Combine(id)
		case Reify(Sym(x),_,_) => Ignore(id)
		case _ => throw new RuntimeException("todo")
	}
	
	def getInputs(x : Any) : List[Int] = x match {
		case VectorFlatten(Sym(x1), Sym(x2)) => List(x1, x2)
		case VectorMap(Sym(x), _) => List(x)
		case VectorFlatMap(Sym(x), _) => List(x)
		case Reflect(VectorSave(Sym(x), _),_,_) => List(x)
		case Reify(Sym(x),_,_) => List(x)
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

			def visitedFilter(node : graph.NodeT) = 
					starts.contains(node.value) || gbks.contains(node.value)

			def makeEdgeFilter(node : graph.NodeT) = {
				thisNode : graph.EdgeT => thisNode.from.value match {
				case x : Any if x == node.value => true
				case GroupByKey(_) => false
				case _ => true
				}
			}

			def goBackward(node : graph.NodeT) {
				node.traverseNodes(direction = Predecessors, edgeFilter = makeEdgeFilter(node)
						, nodeFilter = makeFilter(node.value)){
					x =>
//					println("backward "+x.value)
					x.value match {
					case GroupByKey(_) if (x.value != node.value)=> {println("Hi there"); Cancel}
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
					case gbk@GroupByKey(_) if gbks.contains(gbk)=> Cancel
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
				// make a mapper
				val starts = findAllRelated(head)
				toDo --= starts
				val mappersAndGBKs = starts.map(x => makeMapper(graph.get(x)))
				val gbks = mappersAndGBKs.map(_._1).reduce(_++_)
				val mappers = mappersAndGBKs.map(_._2)
				val groups = gbks.map{ x => new GroupByKeyPart(x)}
	
				val reducers = gbks.map{
					x =>
	
					var reducer = graph.get(x).diSuccessors.head.value
					var nodes = Set[Node]()
					graph.get(x).traverseNodes(edgeFilter = _.to.value match {case GroupByKey(_) => false; case _ => true}) {
						x=>
						nodes += x;
						Continue
					}
					nodes -= x
							val red = new Reducer(reducer)
					nodes.foreach(_.mscrPart += red)
					red
				}
				out += new Mscr(mappers.toBuffer, reducers.toBuffer, groups.toBuffer)
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
				sw.write("%s -> %s;\n".format(edge.from.id, edge.to.id))
			}
			sw.write("}")
			sw.toString()
		}
	
	
		override def toString() = "GraphState "
	}
	object GraphState extends ThreadLocal[GraphState]{
		override def get = {
		  var out = super.get
		  if (out.equals(null)) {
		    out = new GraphState()
		    super.set(out)
		  }
		  out
		}
	}
	
	def graphState = GraphState.get()
	
	override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
		GraphState.set(new GraphState)
		val out = super.emitSource(f, className, stream)
		//    graphState.finish
		System.out.println(graphState.map)
		FileOutput.writeln(graphState.export)
		out
	}
	
	override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
		//	System.out.println("%s to %s".format(sym, rhs))
	
		val op = findDefinition(sym).get.rhs
				val partnerNode = {
			if (graphState.map.contains(sym.id))
				graphState.map(sym.id)
				else
					getPartnerNode(sym.id, op)
		}
		graphState.map(sym.id) = partnerNode
				val otherAttributes = getOtherAttributes(op).map( x=> "%s=%s".format(x._1, x._2)).mkString(",")
				//    FileOutput.writeln("""%s [label="%s",%s];""".format(sym.id, getName(op), otherAttributes))
				for (x <- getInputs(rhs)) {
					val node = graphState.map(x)
							graphState.builder += node ~> partnerNode
				}
	
		super.emitNode(sym, rhs)
		//	System.out.println("%s from inputs %s".format(sym, getInputs(rhs)))
	}

}
