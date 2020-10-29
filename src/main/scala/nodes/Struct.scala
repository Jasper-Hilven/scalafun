package nodes

import types.TypeConstraint

case class Struct(
                   next: BigInt,
                   funccalls: Map[FuncCall, FuncCallData],
                   funcdefs: Map[FuncDef, FuncDefData],
                   scopecontainers: Map[ScopeContainer, ScopeContainerData],
                   constants: Map[Constant, ConstantData],
                   namespaces: Map[Namespace, NamespaceData],
                   nodes: Map[Node, NodeData],
                   parameters: Map[Parameter, ParameterData],
                   sexprs: Map[SExpr, SExprData],
                   baseFuncs: Map[BaseFunc, BaseFuncData],
                   deltaMap: Map[BigInt, Delta]) {

  def this() = {
    this(0, Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map())
  }

  def getAll() = nodes.keys;

  def increaseNext() = copy(next = this.next + 1)

  private def addEmptySExprData(id: SExpr) = {
    copy(next = next + 1,
      nodes = nodes + (id -> NodeData(null)),
      sexprs = sexprs + (id -> SExprData(null, Set(), Set())))
  }

  def addEmptyFuncDef(): (Struct, FuncDef) = {
    val fd = new FuncDef(next)
    (addEmptySExprData(fd).copy(
      funcdefs = funcdefs + (fd -> FuncDefData(List(), null)),
      scopecontainers = scopecontainers + (fd -> ScopeContainerData(Set()))), fd)
  }

  def addFuncDef(parentScope: ScopeContainer, name: String) = {
    val (u0, funcdef) = addFuncDef(parentScope);
    (funcdef.setName(u0, name), funcdef)
  }

  def addFuncDef(parentScope: ScopeContainer) = {
    val (u0, fdef) = addEmptyFuncDef();
    (parentScope.addChild(u0, fdef), fdef)
  }

  def addEmptyFunccall(): (Struct, FuncCall) = {
    val fd = new FuncCall(next)
    (addEmptySExprData(fd).copy(funccalls = funccalls + (fd -> FuncCallData(List()))), fd)
  }

  def addFuncCall(parentScope: ScopeContainer, function: FuncDef) = {
    val (u0, funccall) = addEmptyFunccall()
    val u1 = parentScope.addChild(u0, funccall)
    (funccall.addAsArgument(u1, function), funccall)
  }

  def addEmptyNamespace(): (Struct, Namespace) = {
    val fd = new Namespace(next)
    (copy(next = next + 1,
      scopecontainers = scopecontainers + (fd -> ScopeContainerData(Set())),
      nodes = nodes + (fd -> NodeData(null)),
      namespaces = namespaces + (fd -> NamespaceData(Set(), null))
    ), fd)
  }

  def addEmptyParameter(): (Struct, Parameter) = {
    val fd = new Parameter(next)
    (addEmptySExprData(fd).copy(parameters = parameters + (fd -> ParameterData(null))), fd)
  }

  def addConstant(value: ConstantValue): (Struct, Constant) = {
    val fd = new Constant(next)
    (addEmptySExprData(fd).copy(constants = constants + (fd -> ConstantData(value))), fd)
  }

  def addBaseFunc(parameters: List[Parameter], types: TypeConstraint) = {
    val bf = new BaseFunc(next)
    (addEmptySExprData(bf).copy(baseFuncs = baseFuncs + (bf -> BaseFuncData(parameters, types))), bf)
  }

}


