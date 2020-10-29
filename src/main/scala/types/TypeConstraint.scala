package types

import nodes.Struct

trait TypeConstraint {
  def id: BigInt;
}

case class BooleanConstraint(id: BigInt) extends TypeConstraint

case class BooleanConstraintData(fixedValue: Option[Boolean])

case class IntConstraint(id: BigInt) extends TypeConstraint

case class IntConstraintData(fixedValue: Option[Int])

case class StringConstraint(id: BigInt) extends TypeConstraint

case class StringConstraintData(fixedValue: Option[String])

case class ObjectConstraint(id: BigInt) extends TypeConstraint

case class ObjectConstraintData(map: Map[String, TypeConstraint])

case class OrConstraint(id: BigInt) extends TypeConstraint

case class OrConstraintData(possibilities: List[TypeConstraint])

case class AndConstraint(id: BigInt) extends TypeConstraint

case class AndConstraintData(combinations: List[TypeConstraint])

case class GenericConstraint(id: BigInt) extends TypeConstraint

case class GenericConstraintData()

case class FunctionConstraint(id: BigInt) extends TypeConstraint

case class FunctionConstraintData(input: List[TypeConstraint], output: TypeConstraint)

object typechecker {
  def evaluate(struct: Struct) = {

  }
}
