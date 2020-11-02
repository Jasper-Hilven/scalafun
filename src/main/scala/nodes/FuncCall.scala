package nodes

case class FuncCall(id: BigInt) extends SExpr {
  def replaceArgumentAt(struct: Struct, position: Int, newToUse: SExpr) =
    setArgumentAt(removeArgumentAt(struct, position), position, newToUse)

  def addAsArgument(struct: Struct, argument: SExpr) = {
    setArgumentAt(struct, getAmountOfArguments(struct), argument)
  }

  def setArgumentAt(struct: Struct, position: Int, argument: SExpr) = {
    val updatedS = argument.updateSExpr(struct, (sE) => sE.copy(usedAsArgument = sE.usedAsArgument + ArgumentUsage(this, position)))
    updateFC(updatedS, (fData) => fData.copy(argumentMap = fData.argumentMap.updated(position, argument)))
  }

  def removeArgumentAt(struct: Struct, position: Int) = {
    val argument = getArgumentAt(struct, position);
    val updatedS = argument.updateSExpr(struct, (sE) => sE.copy(usedAsArgument = sE.usedAsArgument - ArgumentUsage(this, position)))
    updateFC(updatedS, fData => fData.copy(argumentMap = fData.argumentMap.map(sE => if (sE != argument) sE else null)))
  }

  def getAmountOfArguments(struct: Struct) = struct.funccalls(this).argumentMap.size;

  def updateFC(struct: Struct, updateF: FuncCallData => FuncCallData) = struct.updateFuncCall(this, updateF)

  def getArgumentAt(struct: Struct, index: Int) = struct.funccalls(this).argumentMap(index)
}

case class FuncCallData(argumentMap: List[SExpr])


case class ArgumentUsage(funccall: FuncCall, position: Int)


case class CreateFuncCall(id: FuncCall, delta: FuncCallData) extends Delta

case class UpdateFuncCall(id: FuncCall, delta: FuncCallData) extends Delta

case class DeleteFuncCall(id: FuncCall) extends Delta
