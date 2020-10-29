package nodes


case class Constant(id: BigInt) extends SExpr

case class ConstantData(value: ConstantValue)

trait ConstantValue;

case class IntConstantValue(value: Int) extends ConstantValue;

case class StringConstantValue(value: String) extends ConstantValue;

case class CharConstantValue(value: Char) extends ConstantValue;

case class BooleanConstantValue(value: Boolean) extends ConstantValue;

case class CreateConstant(id: Constant, delta: ConstantData) extends Delta

case class UpdateConstant(id: Constant, delta: ConstantData) extends Delta

case class DeleteConstant(id: Constant) extends Delta
