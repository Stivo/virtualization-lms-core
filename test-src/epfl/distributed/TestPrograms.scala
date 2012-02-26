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
    //RandomVector(7) + (ZeroVector(7) + RandomVector(7))
    val words2 = Vector("words1")
    val words1 = Vector("words2")
    val words = words1//++words2
    val wordsInLine = words//.flatMap( _.split(" ").toSeq)
//    words.map(_.contains(" ")).save("lines with more than one word")
    val firstWordsInLine = words.map(_.split(" ").apply(0)).save("firstwords")
    val wordsTupled = wordsInLine.map((_, unit(1)))
    val wordsGrouped = wordsTupled.groupByKey
    wordsGrouped.reduce(_+_).save("counts")
    val distinct = wordsGrouped.reduce((x,y) => x).map(_._1)
    distinct.save("distinct")
    val logEntries = Vector("logs")
    logEntries.save("asdf")
//    wordsTupled.save("wordsTupled")
    unit(())
  }
  
   def twoStage(x: Rep[Unit]) = {
    //RandomVector(7) + (ZeroVector(7) + RandomVector(7))
    val words = Vector("words2")
    val wordsInLine = words.flatMap( _.split(" ").toSeq)
//    words.map(_.contains(" ")).save("lines with more than one word")
    val wordsTupled = wordsInLine.map((_, unit(1)))
    val wordsGrouped = wordsTupled.groupByKey
    val counted = wordsGrouped.reduce(_+_)
    val inverted = counted.map(x => (x._2,x._1))
    inverted.map(x => (unit(0), x)).groupByKey.reduce((x,y) => if (x._1 > y._1) x else y).save("asdf")
    val invertedGrouped = inverted.groupByKey
//    val invertedGrouped2 = Vector[(Int, String)]("Asdf").groupByKey
    
//    val added = invertedGrouped++invertedGrouped2
    invertedGrouped
//    invertedGrouped.save("inverted")
  }

  
  class HadoopContext() {
    def getPath() : Rep[String] = unit("asdf")
  }
  
  def logAnalysis(x : Rep[Unit]) = {
    val inputPath = "asf"
    val lines = Vector(inputPath)
    val years = lines.flatMap(_.split(" ").toSeq).flatMap(_.split("/").toSeq)
		.filter(_.matches("18\\d{2}"))
	years.save("years")
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

      val dsl = new VectorsProg with VectorOpsExp with VectorImplOps 
//      val codegen = new ScalaGenFunctions with ScalaGenUtil with ScalaGenVector with ScalaGenBase { val IR: dsl.type = dsl }
//      codegen.emitSource(dsl.test, "g", new PrintWriter(System.out))
      val toCompile = dsl.simple _
      val codegenHadoop = new HadoopGen { val IR: dsl.type = dsl }
      codegenHadoop.emitSource(toCompile, "Hadoop", new PrintWriter(System.out))

      val dslSpark = new VectorsProg with VectorOpsExp with VectorImplOps with SparkVectorOpsExp with SparkVectorOpsExpOpt
      //      val codegenDeps = new HadoopGen { val IR: dsl.type = dsl }
//      codegenDeps.emitSource(dsl.simple, "g", new PrintWriter(System.out))
      val codegenSpark = new SparkGen { val IR: dslSpark.type = dslSpark }
      codegenSpark.emitSource(dslSpark.wordCount, "Spark", new PrintWriter(System.out))

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
