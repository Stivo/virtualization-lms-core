package epfl.distributed

//import java.io.DataOutput
//import java.io.DataInput
//import org.apache.hadoop.io.Writable
//import org.apache.hadoop.io.WritableComparable
//import org.apache.hadoop.io.WritableUtils
//import org.apache.hadoop.io.WritableComparator
//import scala.xml.Text
//import org.apache.hadoop.io.IntWritable

//class TaggedValue extends Writable {
//  var tag : Byte = -1
//  var value : Array[Byte] = null
//  def readFields(input : DataInput) {
//    tag = input.readByte()
//    val length = WritableUtils.readVInt(input)
//    value = new Array[Byte](length)
//    input.readFully(value)
//  }
//  def write(output : DataOutput) {
//    output.writeByte(tag)
//    // TODO optimize: If type is statically known and does not need a length info
//    WritableUtils.writeVInt(output, value.length)
//    output.write(value)
//  }
//
//}
//
//class TaggedKey extends TaggedValue with WritableComparable[TaggedKey] {
//  override def compare(other : TaggedKey) = {
//    val tagDiff = tag-other.tag
//    if (tagDiff != 0)
//      tagDiff
//      else
//        WritableComparator.compareBytes(value, 0, value.length-1, 
//        							  other.value, 0, other.value.length-1)
//  }
//  
//}
//
//
//class HadoopType[Key: Manifest, Value : Manifest]
//		(val keyTagged : Boolean = false, val taggedValue : Boolean = false) {
//
//  def readFrom(key : String, value : String) = {
//    getHadoopKeyType
//  }
//  
//  def getHadoopKeyType = {
//    if (keyTagged) {
//      classOf[TaggedKey]
//    } else {
//      manifest[Key].erasure.getName match {
//        case "java.lang.String" => classOf[Text]
//        case "int" => classOf[IntWritable]
//        case "Int" => classOf[IntWritable]
//      }
//    }
//  }
//  
//  // needs code to:
//  // convert to these types 
//  // convert from hadoop types
//  
//  
//}