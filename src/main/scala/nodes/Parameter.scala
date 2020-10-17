package nodes


case class Parameter(id: BigInt) extends SExpr {
  def updateParam(struct: Struct, updateF: ParameterData => ParameterData): Struct =
    struct.copy(parameters = struct.parameters + (this -> updateF(struct.parameters(this))))

  def addToFunction(struct: Struct, function: FuncDef): Struct =
    function.updateFD(updateParam(struct, (pd) => pd.copy(function = function)),
      fd => fd.copy(parameters = fd.parameters :+ this))
}

case class ParameterData(function: FuncDef)