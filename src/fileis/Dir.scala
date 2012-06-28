package fileis

import java.io.{ByteArrayOutputStream, OutputStream, File}
import FileNode._

/**
 * @author jkee
 */

class Dir(sourceName: String)(
           var nodes: Array[FileNode]
           ) extends FileNode(sourceName) {

  override def toString = {
    val stream = new ByteArrayOutputStream()
    printTo(stream)
    new String(stream.toByteArray)
  }

  override def update(path: String): FileNode = {
    if (path != null && !isDirectoryReal(path)) new FileLeaf(name)
    val fileList = listFiles(path, name)
    if (fileList == null) {
      if (nodes.isEmpty) this
      else new Dir(name)(Array())
    }
    else if (fileList.length == nodes.length && nodes.forall(node => fileList.exists(_.getName == node.name)) ) {
      nodes = nodes.map(_.update(makePath(path)))
      this
    }
    else {
      val newFiles = fileList.filter(file => !nodes.exists(_.name == file.getName))
      val newNodes = newFiles.map(evaluateFileNode(_)
      val oldExisted: Array[FileNode] = nodes.filter(node => fileList.exists(node.name == _.getName))
      val nodesFull: Array[FileNode] = newNodes ++ oldExisted.map(_.update(makePath(path)))
      new Dir(name)(nodesFull)
    }
  }

  override def printTo(stream: OutputStream, level: Int) {
    super.printTo(stream, level, "++")

    for (node <- nodes) {
      stream.write('\n')
      node.printTo(stream, level + 1)
    }
  }


  private def isDirectoryReal(path: String) = new File(path, name).isDirectory

}
