package types

import nodes.{Delta, Namespace, Node, Struct}

import scala.collection.immutable.MultiDict


case class TypeDependencyGraph(multiDict: MultiDict[Node, Node]) {

  def doOperation(currentStruct: Struct,
                  nextStruct: Struct,
                  delta: Delta) = {

    multiDict + (Namespace(1), Namespace(1))
  }
}
