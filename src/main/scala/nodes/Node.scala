package nodes


trait Node {
  def id: BigInt;

  def updateNode(struct: Struct, update: NodeData => NodeData): Struct =
    struct.copy(nodes = struct.nodes + (this -> update(struct.nodes(this))))

  def withNode(struct: Struct, nodeData: NodeData): Struct = struct.copy(nodes = struct.nodes + (this -> nodeData))

  def addTo(struct: Struct, nodeData: NodeData): Struct = withNode(struct, nodeData)

  def setName(struct: Struct, name: String) = updateNode(struct, d => d.copy(name = name))
}


case class NodeData(name: String)


trait NodeDataBuild {
  def create(id: Node): NodeData
}


case class CreateNode(id: Node, delta: NodeData) extends Delta

case class UpdateNode(id: Node, delta: NodeData) extends Delta

case class DeleteNode(id: Node) extends Delta
