package fileis

import java.io.{ByteArrayOutputStream, OutputStream, File}


/**
 * @author jkee
 */

abstract class FileNode(
                         val name:String
                         ) {

  override def toString = {
    val stream = new ByteArrayOutputStream()
    printTo(stream)
    new String(stream.toByteArray)
  }

  final def makePath(path: String): String = path + name

  def update(path: String):FileNode = this



  final def printTo(stream: OutputStream) {
    printTo(stream, 0)
  }

  def printTo(stream: OutputStream, level: Int) {
    printTo(stream, level, "--")
  }

  protected def printTo(stream: OutputStream, level: Int, sign:String) {
    stream.write((sign * level + name).getBytes)
  }

}

object FileNode {

  def listFiles(path: String, name: String):Array[File] = new File(path, name).listFiles()

  def fileArray(name: String):Array[FileNode] = fileArray(new File(name))
  def fileArray(root: File):Array[FileNode] = {
    val fileList = root.listFiles()
    if (fileList == null) Array()
    else for (node <- root.listFiles()) yield evaluateFileNode(node)
  }

  def evaluateFileNode(file: File): FileNode = {
    if (!file.isDirectory) new FileLeaf(file.getName)
    else new Dir(file.getName)(fileArray(file))
  }

  def buildFileTree(root:String): FileNode = {
    evaluateFileNode(new File(root))
  }

  def main(args: Array[String]) {
    val time = System.currentTimeMillis()
    val root = buildFileTree(args(0))
    //Uncomment to print file tree
    //root.printTo(System.out); println()
    println("Time: " + (System.currentTimeMillis() - time))
  }
}
