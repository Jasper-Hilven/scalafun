package nodes


case class Namespace(id: BigInt) extends ScopeContainer {
  def updateNs(struct: Struct, updateF: NamespaceData => NamespaceData): Struct = struct.updateNamespace(this, updateF)

  def setParent(struct: Struct, parent: Namespace) =
    updateNs(parent.updateNs(struct, pD => pD.copy(namespaceChildren = pD.namespaceChildren + this)),
      d => d.copy(parentNamespace = parent))
}

case class NamespaceData(namespaceChildren: Set[Namespace],
                         parentNamespace: Namespace)

case class CreateNamespace(id: Namespace, delta: NamespaceData) extends Delta

case class UpdateNamespace(id: Namespace, delta: NamespaceData) extends Delta

case class DeleteNamespace(id: Namespace) extends Delta

