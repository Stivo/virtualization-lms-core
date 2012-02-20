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
  
  def wordCount(x: Rep[Unit]) = {
    //RandomVector(7) + (ZeroVector(7) + RandomVector(7))
    val words2 = Vector[String]("words1")
    val words1 = Vector[String]("words2")
    val words = words1//++words2
    val wordsInLine = words.flatMap( _.split(" ").toSeq)
//    words.map(_.contains(" ")).save("lines with more than one word")
    val firstWordsInLine = words.map(_.split(" ").apply(0)).save("firstwords")
    val wordsTupled = wordsInLine.map((_, unit(1)))
    val wordsGrouped = wordsTupled.groupByKey
    wordsGrouped.reduce(_+_).save("counts")
    wordsGrouped.map(_._1).save("distinct")
    val logEntries = Vector[String]("logs")
//    logEntries
//    logEntries.map(_.contains(" ")).save("asdf")
//    v1.save("allWords")
    wordsTupled.save("wordsTupled")
    //v1
//    v1.map(_+1).flatten(v1).save("flattened")
//    v2.save("test")
//    v1.map(x : Int => "")
//    v3.map(Integer.parseInt(_)).flatten(v1)
//    v2.map(_.startsWith("asdf")).map(!_)
  }
  
}



class TestVectors extends FileDiffSuite {
  
  def testVectors {
//    withOutput(System.err/*"test-out/epfl/test-dist"*/) {    
      println("-- begin")

      val dsl = new VectorsProg with VectorOpsExp with VectorImplOps 
//      val codegen = new ScalaGenFunctions with ScalaGenUtil with ScalaGenVector with ScalaGenBase { val IR: dsl.type = dsl }
//      codegen.emitSource(dsl.test, "g", new PrintWriter(System.out))
      val codegenDeps = new HadoopGen { val IR: dsl.type = dsl }
      codegenDeps.emitSource(dsl.wordCount, "g", new PrintWriter(System.out))
      val graph = new GraphVizExport { val IR: dsl.type = dsl }
//      val r = { val x = fresh; x+1 }
//      graph.emitDepGraph(r, "test.dot")
//      new StringsProg with VectorsExp with VectorsImplExternal
//      with CompileScala { self =>
//        val codegen = new ScalaGenFunctions with ScalaGenUtil { val IR: self.type = self }
//        codegen.emitSource(test, "Test", new PrintWriter(System.out))
//        val g = compile(test)
//        println(g(0))
//      }

      println("-- end")
//    }
//      FileOutput.finish
//      println(FileOutput.sw.toString)
      
    //assert(true, "did finish")    
  }
}