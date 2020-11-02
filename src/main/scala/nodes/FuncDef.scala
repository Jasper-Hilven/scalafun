package nodes


case class FuncDef(id: BigInt) extends SExpr with ScopeContainer {
  def updateFD(struct: Struct, updateF: FuncDefData => FuncDefData) =
    struct.updateFuncDef(this, updateF)

  def setResult(struct: Struct, result: SExpr): Struct =
    result.updateSExpr(updateFD(struct, d => d.copy(functionResult = result)),
      sE => sE.copy(usedAsFunctionResult = sE.usedAsFunctionResult + this))

  def getResult(struct: Struct) = struct.funcdefs(this).functionResult;

  def removeResult(struct: Struct): Struct =
    getResult(struct).updateSExpr(updateFD(struct, d => d.copy(functionResult = null)),
      sE => sE.copy(usedAsFunctionResult = sE.usedAsFunctionResult - this))

  def replaceResult(struct: Struct, newResult: SExpr): Struct = {
    setResult(removeResult(struct), newResult);
  }

  def addNewParameter(struct: Struct, parameterName: String) = {
    val (u0, param) = struct.addEmptyParameter();
    val u1 = param.setName(u0, parameterName)
    val u2 = addChild(u1, param)
    (param.addToFunction(u2, this), param)
  }

  def addNewParameters(struct: Struct, parameterNames: List[String]): Struct = {
    parameterNames.foldLeft(struct, (struct: Struct, name: String) => addNewParameter(struct, name)._1)
  }
}

case class FuncDefData(parameters: List[Parameter],
                       functionResult: SExpr)


case class CreateFuncDef(id: FuncDef, delta: FuncDefData) extends Delta

case class UpdateFuncDef(id: FuncDef, delta: FuncDefData) extends Delta

case class DeleteFuncDef(id: FuncDef) extends Delta
