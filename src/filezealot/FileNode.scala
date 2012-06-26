package filezealot

import java.io.File


/**
 * @author jkee
 */

abstract class FileNode(
                         val name:String
                         ) {

  override def toString = name

  final def makePath(path: String): String = path + name

  def update(path: String):FileNode = this

}

object FileNode {

  def listFiles(path: String, name: String):Array[File] = new File(path, name).listFiles()

  def fileArray(name: String):Array[FileNode] = fileArray(new File(name))
  def fileArray(root: File):Array[FileNode] = for (node <- root.listFiles()) yield evaluateFileNode(node)

  def evaluateFileNode(file: File): FileNode = {
    if (!file.isDirectory) new FileLeaf(file.getName)
    else new Dir(file.getName)(fileArray(file))
  }

  def buildFileTree(root:String): FileNode = {
    evaluateFileNode(new File(root))
  }

  def main(args: Array[String]) {
    val time = System.currentTimeMillis()
    println(buildFileTree("/home/jkee"))
    println("Time: " + (System.currentTimeMillis() - time))
  }
}
