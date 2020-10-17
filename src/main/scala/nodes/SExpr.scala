package nodes


trait SExpr extends Node {

  def getParentScope(struct: Struct): ScopeContainer = struct.sexprs(this).parentScope;

  def getArgumentUsages(struct: Struct): Set[ArgumentUsage] = struct.sexprs(this).usedAsArgument;

  def getFunctionResultUsages(struct: Struct): Set[FuncDef] = struct.sexprs(this).usedAsFunctionResult;

  def updateSExpr(struct: Struct, update: SExprData => SExprData): Struct =
    struct.copy(sexprs = struct.sexprs + (this -> update(struct.sexprs(this))))

  def replaceAllUsagesWith(struct: Struct, newToUse: SExpr): Struct = {
    val u0: Struct = getFunctionResultUsages(struct).foldLeft(struct, (acc: Struct, funcresUs: FuncDef) => funcresUs.replaceResult(acc, newToUse))
    getArgumentUsages(u0).foldLeft(u0, (acc: Struct, argUs: ArgumentUsage) => argUs.funccall.replaceArgumentAt(acc, argUs.position, newToUse))
  }

  def replaceWithDirectFunctionCall(struct: Struct) {
    val parentScope = getParentScope(struct);
    val (u0, function) = struct.addEmptyFuncDef()
    val u1 = parentScope.addChild(u0, function)
    val (u2, funccall) = u1.addFuncCall(parentScope, function);
    val u3 = replaceAllUsagesWith(u2, funccall)
    function.setResult(u3, this)
  }

}

case class SExprData(parentScope: ScopeContainer,
                     usedAsArgument: Set[ArgumentUsage],
                     usedAsFunctionResult: Set[FuncDef])
