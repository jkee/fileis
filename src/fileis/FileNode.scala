package fileis

import java.io.{ByteArrayOutputStream, OutputStream, File}
import FileNode._
import java.util
import java.awt.image.DirectColorModel


/**
 * @author jkee
 */

abstract class FileNode(
                         sourceName:String
                         ) {

  val name:String = if (sourceName.isEmpty) "/" else sourceName

  override def toString = {
    val stream = new ByteArrayOutputStream()
    printTo(stream)
    new String(stream.toByteArray)
  }

  final def makePath(path: String): String = if (path == null) name else path + name

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
    else if (isSymlink(file)) new SymLink(file.getName)
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

  def bfsSearch(root: FileNode, predicate: FileNode => Boolean): FileNode = {
    val fifo = new util.ArrayDeque[FileNode]()
    var currentFile = root
    while(currentFile != null && !predicate(currentFile)) {
      currentFile match {
        case currentFile:Dir => currentFile.nodes.foreach(fifo.offer(_))
        case _ =>
      }
      if (fifo.isEmpty) currentFile = null
      else currentFile = fifo.poll()
    }
    currentFile
  }

  //Apache commons
  def isSymlink(file: File):Boolean = {
    if (file == null) throw new NullPointerException
    val canon:File = {
      if (file.getParent == null) file
      else new File(file.getParentFile.getCanonicalFile, file.getName)
    }
    !canon.getCanonicalFile.equals(canon.getAbsoluteFile)
  }

}
