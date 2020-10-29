package nodes

import types.TypeConstraint

case class BaseFunc(id: BigInt) extends SExpr

case class BaseFuncData(parameters: List[Parameter], types: TypeConstraint)

case class CreateBaseFunc(id: BaseFunc, delta: BaseFuncData) extends Delta

case class UpdateBaseFunc(id: BaseFunc, delta: BaseFuncData) extends Delta

case class DeleteBaseFunc(id: BaseFunc) extends Delta
