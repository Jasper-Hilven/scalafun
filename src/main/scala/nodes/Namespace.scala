package nodes



case class Namespace(id: BigInt) extends ScopeContainer {
  def updateNs(struct: Struct, updateF: NamespaceData => NamespaceData): Struct =
    struct.copy(namespaces = struct.namespaces + (this -> updateF(struct.namespaces(this))))

  def setParent(struct: Struct, parent: Namespace) =
    updateNs(parent.updateNs(struct, pD => pD.copy(namespaceChildren = pD.namespaceChildren + this)),
      d => d.copy(parentNamespace = parent))
}

case class NamespaceData(namespaceChildren: Set[Namespace],
                         parentNamespace: Namespace)
