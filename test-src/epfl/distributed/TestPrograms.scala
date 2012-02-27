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

trait VectorsProg extends VectorImplOps {
  
  // why need of unit in this example?
  // why is the code in the CodeGen lifted???
  // How to implement map in terms of flatMap?
  // use ParallelDo instead of flatMap? Has notion of emitter
  
  def parse( s: Rep[String]) = Integer.parseInt(s)
  
  def isNumber(s : Rep[String]) = s.matches("\\d+")
  
  def simple(x : Rep[Unit]) = {
    val words1 = Vector("words1")
//    words1.filter(_.matches("\\d+"))
    val parsed = words1.map(parse)
//    .filter(_>4)
    parsed.map{x => 2*x}.save("doubled")
    parsed.map{x => x+1}.save("incremented")
//    .map(_+"asdf")
//    .save("words")
    unit("348")
    //unit(())
  }
  
  def wordCount(x: Rep[Unit]) = {
    val args = Vector.getArgs
	val output = args(1)
	val lines = Vector(args(0))
    val wordsInLine = lines.map( _.split("\\s+").apply(1)).flatMap(_.split("[:/_.,]").toSeq)
//    words.map(_.contains(" ")).save("lines with more than one word")
    val wordsTupled = wordsInLine.map((_, unit(1)))
    val counted = wordsTupled.groupByKey.reduce(_+_).filter(_._2 > 10)
    counted.save(output+"/wordcounts")
    val inverted = counted.map(x => (x._2,x._1))
//    val reduced = inverted.reduce((x,y) => if (x._1 > y._1) x else y)
//    println(reduced)
    val invertedSameKey = inverted.map(x => (unit(0), x))
    invertedSameKey.groupByKey.reduce((x,y) => if (x._1 > y._1) x else y)
    .map(_._2)
    .save(output+"/most common word")
  }
  
   def twoStage(x: Rep[Unit]) = {
    //RandomVector(7) + (ZeroVector(7) + RandomVector(7))
    val words = Vector("words2")
    val wordsInLine = words//.flatMap( _.split(" ").toSeq)
//    words.map(_.contains(" ")).save("lines with more than one word")
    val wordsTupled = wordsInLine.map((_, unit(1)))
    val wordsGrouped = wordsTupled.groupByKey
    val counted = wordsGrouped.reduce(_+_)
    counted.save("wordcounts")
    val inverted = counted.map(x => (x._2,x._1))
    inverted.map(x => (unit(0), x))
    .groupByKey
    .reduce((x,y) => if (x._1 > y._1) x else y)
    .map(_._2)
    .save("most common word")
//    val invertedGrouped = inverted.groupByKey
//    val invertedGrouped2 = Vector[(Int, String)]("Asdf").groupByKey
    
//    val added = invertedGrouped++invertedGrouped2
//    invertedGrouped
//    invertedGrouped.save("inverted")
  }

  
  class HadoopContext() {
    def getPath() : Rep[String] = unit("asdf")
  }
  
  def logAnalysis(x : Rep[Unit]) = {
    val args = Vector.getArgs
    val lines = Vector(args(0))
    val years = lines.flatMap(_.split(" ").toSeq).flatMap(_.split("/").toSeq)
		.filter(_.matches("18\\d{2}"))
	years.save(args(1))
	unit(())
  }
  def testFusion(x : Rep[Unit]) = {
    val args = Vector.getArgs
    val lines = Vector(args(0))
    val years = lines.map{y => val x = y.split("\\s+");x(x.length-1)}.map(Integer.parseInt).map(_%7 == 1)
	years.save(args(1))
	unit(())
  }
  def trends(x : Rep[Unit]) = {
    val context = new HadoopContext()
    val inputPath = "asf"
    val lines = Vector(inputPath)
    val years = lines.map((context.getPath(),_))
	years.save("years")
	unit(())
  }
  
}



class TestVectors extends FileDiffSuite {
  
  def testVectors {
//    withOutput(System.err/*"test-out/epfl/test-dist"*/) {    
    try {
      println("-- begin")

      val dsl = new VectorsProg with VectorOpsExp 
//      val codegen = new ScalaGenFunctions with ScalaGenUtil with ScalaGenVector with ScalaGenBase { val IR: dsl.type = dsl }
//      codegen.emitSource(dsl.test, "g", new PrintWriter(System.out))
      val toCompile = dsl.twoStage _
      val codegenHadoop = new HadoopGen { val IR: dsl.type = dsl }
      codegenHadoop.emitSource(toCompile, "Hadoop", new PrintWriter(System.out))

      
      val writer = new StringWriter()
      val printer = new PrintWriter(writer)
      
      val dslSpark = new VectorsProg with SparkProgram
      //      val codegenDeps = new HadoopGen { val IR: dsl.type = dsl }
//      codegenDeps.emitSource(dsl.simple, "g", new PrintWriter(System.out))
      val codegenSpark = new SparkGen { val IR: dslSpark.type = dslSpark }
      codegenSpark.emitSource(dslSpark.testFusion, "Spark", printer)
      println(writer.toString)
      val dest = "/home/stivo/master/spark/examples/src/main/scala/spark/examples/SparkGenerated.scala"
      val fw = new FileWriter(dest)
      fw.write(writer.toString)
      fw.close
      println("-- end")
//    }
//      FileOutput.finish
//      println(FileOutput.sw.toString)
    } catch {
      case e => 
        e.printStackTrace
        println(e.getMessage)
    }
    //assert(true, "did finish")    
  }
}
