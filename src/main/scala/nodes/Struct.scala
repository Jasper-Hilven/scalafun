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
                   previousStruct: Struct,
                   delta: Delta) {

  def this() = {
    this(0, Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), null, null)
  }

  //NEXT
  def increaseNext() = copy(next = this.next + 1)


  //DELTA
  private def addDelta(previous: Struct, delta: Delta) = previous.copy(
    previousStruct = previous,
    delta = delta
  )

  private def addDelta(delta: Delta) = copy(
    previousStruct = this,
    delta = delta
  )

  //NODES

  def updateNode(node: Node, update: NodeData => NodeData): Struct = {
    val updatedData = update(nodes(node))
    copy(nodes = nodes + (node -> updatedData))
      .addDelta(UpdateNode(node, updatedData))
  }


  private def addEmptyNode(id: Node) = {
    copy(nodes = nodes + (id -> NodeData(null)))
      .addDelta(CreateNode(id, NodeData(null)))
  }

  def getAll() = nodes.keys;

  //SEXPR
  private def addEmptySExprData(id: SExpr) = {
    val previous = addEmptyNode(id)
    previous.copy(sexprs = sexprs + (id -> SExprData(null, Set(), Set())))
      .addDelta(previous, CreateSExpr(id, SExprData(null, Set(), Set())))
  }

  def updateSExpr(sExpr: SExpr, update: SExprData => SExprData): Struct = {
    val data = update(sexprs(sExpr))
    copy(sexprs = sexprs + (sExpr -> data))
      .addDelta(UpdateSExpr(sExpr, data))
  }

  //SCOPECONTAINER
  private def addEmptyScopeContainerNoNode(id: ScopeContainer) =
    copy(scopecontainers = scopecontainers + (id -> ScopeContainerData(Set())))
      .addDelta(CreateScopeContainer(id, ScopeContainerData(Set())))

  def updateScopeCont(scopeContainer: ScopeContainer, update: ScopeContainerData => ScopeContainerData) = {
    val updatedData = update(scopecontainers(scopeContainer))
    copy(scopecontainers = scopecontainers + (scopeContainer -> updatedData))
      .addDelta(UpdateScopeContainer(scopeContainer, updatedData))
  }

  //FUNCDEF
  def addEmptyFuncDef(): (Struct, FuncDef) = {
    val fd = new FuncDef(next)
    val previous = increaseNext()
      .addEmptySExprData(fd)
      .addEmptyScopeContainerNoNode(fd)
    (previous
      .copy(funcdefs = funcdefs + (fd -> FuncDefData(List(), null)))
      .addDelta(previous, CreateFuncDef(fd, FuncDefData(List(), null))), fd)
  }


  def addFuncDef(parentScope: ScopeContainer, name: String) = {
    val (u0, funcdef) = addFuncDef(parentScope);
    (funcdef.setName(u0, name), funcdef)
  }

  def addFuncDef(parentScope: ScopeContainer) = {
    val (u0, fdef) = addEmptyFuncDef();
    (parentScope.addChild(u0, fdef), fdef)
  }

  def updateFuncDef(funcDef: FuncDef, updateF: FuncDefData => FuncDefData) = {
    val updatedData = updateF(funcdefs(funcDef))
    copy(funcdefs = funcdefs + (funcDef -> updatedData))
      .addDelta(UpdateFuncDef(funcDef, updatedData))
  }


  //FUNCCALL
  def addEmptyFunccall(): (Struct, FuncCall) = {
    val fd = new FuncCall(next)
    val previous = increaseNext().addEmptySExprData(fd)
    (previous
      .copy(funccalls = funccalls + (fd -> FuncCallData(List())))
      .addDelta(previous, CreateFuncCall(fd, FuncCallData(List()))), fd)
  }

  def addFuncCall(parentScope: ScopeContainer, function: FuncDef) = {
    val (u0, funccall) = addEmptyFunccall()
    val u1 = parentScope.addChild(u0, funccall)
    (funccall.addAsArgument(u1, function), funccall)
  }

  def updateFuncCall(funcCall: FuncCall, updateF: FuncCallData => FuncCallData) = {
    val updatedData = updateF(funccalls(funcCall))
    copy(funccalls = funccalls + (funcCall -> updatedData))
      .addDelta(UpdateFuncCall(funcCall, updatedData))
  }

  //NAMESPACE
  def addEmptyNamespace(): (Struct, Namespace) = {
    val fd = new Namespace(next)
    val previous = increaseNext().addEmptyNode(fd).addEmptyScopeContainerNoNode(fd)
    (previous
      .copy(namespaces = namespaces + (fd -> NamespaceData(Set(), null)))
      .addDelta(previous, CreateNamespace(fd, NamespaceData(Set(), null))), fd)
  }

  def updateNamespace(namespace: Namespace, updateF: NamespaceData => NamespaceData): Struct = {
    val updatedData = updateF(namespaces(namespace))
    copy(namespaces = namespaces + (namespace -> updatedData))
      .addDelta(UpdateNamespace(namespace, updatedData))
  }

  //PARAMETER
  def addEmptyParameter(): (Struct, Parameter) = {
    val fd = new Parameter(next)
    val previous = increaseNext()
      .addEmptySExprData(fd)
    (previous
      .copy(parameters = parameters + (fd -> ParameterData(null)))
      .addDelta(previous, CreateParameter(fd, ParameterData(null))), fd)
  }

  def updateParameter(parameter: Parameter, updateF: ParameterData => ParameterData) = {
    val parameterData = updateF(parameters(parameter))
    copy(parameters = parameters + (parameter -> parameterData))
      .addDelta(UpdateParameter(parameter, parameterData))
  }

  //CONSTANT
  def addConstant(value: ConstantValue): (Struct, Constant) = {
    val fd = new Constant(next)
    val previous = increaseNext()
      .addEmptySExprData(fd)
    (previous
      .copy(constants = constants + (fd -> ConstantData(value)))
      .addDelta(previous, CreateConstant(fd, ConstantData(value))), fd)
  }

  //BASEFUNC
  def addBaseFunc(parameters: List[Parameter], types: TypeConstraint) = {
    val bf = new BaseFunc(next)
    val previous = increaseNext()
      .addEmptySExprData(bf)
    (previous
      .copy(baseFuncs = baseFuncs + (bf -> BaseFuncData(parameters, types)))
      .addDelta(previous, CreateBaseFunc(bf, BaseFuncData(parameters, types))), bf)
  }
}


