package nodes


trait Node {
  def id: BigInt;

  def setName(struct: Struct, name: String) = struct.updateNode(this, d => d.copy(name = name))
}


case class NodeData(name: String)


trait NodeDataBuild {
  def create(id: Node): NodeData
}


case class CreateNode(id: Node, delta: NodeData) extends Delta

case class UpdateNode(id: Node, delta: NodeData) extends Delta

case class DeleteNode(id: Node) extends Delta
