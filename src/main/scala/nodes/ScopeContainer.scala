package nodes

trait ScopeContainer extends Node {
  def addChild(struct: Struct, child: SExpr) = {
    val parentScope = child.getParentScope(struct);
    if (parentScope != null) throw new Exception("ParentScope already exists");
    val updatedChild = child.updateSExpr(struct, data => data.copy(parentScope = this))
    updateScopeCont(updatedChild, cont => cont.copy(scopeChildren = cont.scopeChildren + child))
  }

  def removeChild(struct: Struct, child: SExpr) = {
    val parentScope = child.getParentScope(struct);
    if (parentScope != this) throw new Exception("Is not its parentScope");
    val updatedChild = child.updateSExpr(struct, data => data.copy(parentScope = null))
    updateScopeCont(updatedChild, cont => cont.copy(scopeChildren = cont.scopeChildren - child))
  }

  def updateScopeCont(struct: Struct, update: ScopeContainerData => ScopeContainerData) =
    struct.copy(scopecontainers = struct.scopecontainers + (this -> update(struct.scopecontainers(this))))
}

case class ScopeContainerData(scopeChildren: Set[SExpr])


case class CreateScopeContainer(id: ScopeContainer, delta: ScopeContainerData) extends Delta

case class UpdateScopeContainer(id: ScopeContainer, delta: ScopeContainerData) extends Delta

case class DeleteScopeContainer(id: ScopeContainer) extends Delta
