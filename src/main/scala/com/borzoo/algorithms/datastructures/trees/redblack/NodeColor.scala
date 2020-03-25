package com.borzoo.algorithms.datastructures.trees.redblack

sealed trait NodeColor {}

private[redblack] object NodeColor {

  case object Red extends NodeColor

  case object Black extends NodeColor

}
