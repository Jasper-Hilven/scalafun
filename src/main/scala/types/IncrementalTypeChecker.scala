package types

import nodes.{Delta, Struct, UpdateNode}

case class IncrementalTypeChecker(typesDependencyGraph: TypeDependencyGraph) {


  def handleDelta(currentStruct: Struct,
                  nextStruct: Struct,
                  delta: Delta) =
    delta match {
      case UpdateNode(id, delta) => this
      case other => throw new NotImplementedError()
    }
}
