package fileis

import java.io.{OutputStream, File}

/**
 * @author jkee
 */

class Dir(name: String)(
           val nodes: Array[FileNode]
           ) extends FileNode(name) {

  override def toString = "Directory: " + name + "\n" +  nodes.mkString("\n")

  override def update(path: String): FileNode = {
    if (!isDirectoryReal(path)) new FileLeaf(name)
    val fileList = FileNode.listFiles(path, name)
    if (fileList.length == nodes.length && nodes.forall(node => fileList.exists(_.getName == node.name)) ) this
    else {
      val newFiles = fileList.filter(file => nodes.exists(_.name == file.getName))
      val newNodes = newFiles.map(FileNode.evaluateFileNode(_))
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
