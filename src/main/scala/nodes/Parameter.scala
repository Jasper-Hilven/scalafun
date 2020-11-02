package nodes


case class Parameter(id: BigInt) extends SExpr {
  def updateParam(struct: Struct, updateF: ParameterData => ParameterData): Struct =
    struct.updateParameter(this, updateF)

  def addToFunction(struct: Struct, function: FuncDef): Struct =
    function.updateFD(updateParam(struct, (pd) => pd.copy(function = function)),
      fd => fd.copy(parameters = fd.parameters :+ this))
}

case class ParameterData(function: FuncDef)

case class CreateParameter(id: Parameter, delta: ParameterData) extends Delta

case class UpdateParameter(id: Parameter, delta: ParameterData) extends Delta

case class DeleteParameter(id: Parameter) extends Delta
