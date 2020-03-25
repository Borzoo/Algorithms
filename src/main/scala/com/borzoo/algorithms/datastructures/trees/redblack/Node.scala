package com.borzoo.algorithms.datastructures.trees.redblack

private[redblack] case class Node[T](value: Option[T],
                                  var color: NodeColor,
                                  var parent: Node[T],
                                  var left: Node[T],
                                  var right: Node[T]) {
    override def toString: String = value.getOrElse("empty").toString
}
