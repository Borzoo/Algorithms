package com.borzoo.algorithms.datastructures

import scala.annotation.tailrec

class RedBlackTree[T](implicit ord: Ordering[T]) {
  private[datastructures] val nil: Node =
    Node(None, NodeColor.Black, null, null, null)
  private[datastructures] var root: Node = nil

  def all(): Seq[T] = allNodes.map(_.value.get)

  def allNodes: Seq[Node] = root match {
    case `nil` => Seq.empty
    case _     => walk(root)
  }

  private def walk(root: Node): List[Node] = {
    @tailrec
    def walk(nodes: List[(Node, Boolean)], acc: List[Node]): List[Node] =
      nodes match {
        case Nil               => acc
        case (`nil`, _) :: t   => walk(t, acc)
        case (node, true) :: t => walk(t, node :: acc)
        case (node, false) :: t =>
          walk((node.right, false) :: (node, true) :: (node.left, false) :: t,
               acc)
      }

    walk(List((root, false)), List())
  }

  @tailrec
  final def fixColorsAfterInsert(node: Node): Unit = {
    if (node == root)
      node.color = NodeColor.Black

    if (node.parent.color == NodeColor.Red) {
      if (node.parent == node.parent.parent.left) {
        val uncle = node.parent.parent.right
        if (uncle.color == NodeColor.Red) {
          node.parent.parent.color = NodeColor.Red
          uncle.color = NodeColor.Black
          node.parent.color = NodeColor.Black
          fixColorsAfterInsert(node.parent.parent)
        } else {
          if (node == node.parent.right) {
            leftRotate(node.parent)
            fixColorsAfterInsert(node.left)
          } else {
            node.parent.parent.color = NodeColor.Red
            rightRotate(node.parent.parent)
            node.parent.color = NodeColor.Black
            node.color = NodeColor.Red
            fixColorsAfterInsert(root)
          }
        }
      } else {
        val uncle = node.parent.parent.left
        if (uncle.color == NodeColor.Red) {
          node.parent.parent.color = NodeColor.Red
          uncle.color = NodeColor.Black
          node.parent.color = NodeColor.Black
          fixColorsAfterInsert(node.parent.parent)
        } else {
          if (node == node.parent.left) {
            rightRotate(node.parent)
            fixColorsAfterInsert(node.right)
          } else {
            node.parent.parent.color = NodeColor.Red
            leftRotate(node.parent.parent)
            node.parent.color = NodeColor.Black
            node.color = NodeColor.Red
            fixColorsAfterInsert(root)
          }
        }
      }
    }
  }

  def insert(value: T): Unit = {
    if (root == nil) {
      root = Node(Some(value), NodeColor.Black, nil, nil, nil)
    } else {
      var x = root
      var y = if (ord.gt(value, root.value.get)) root.right else root.left
      insert()

      @tailrec
      def insert(): Unit = {
        y match {
          case `nil` =>
            val node = Node(Some(value), NodeColor.Red, x, nil, nil)
            if (ord.gt(value, x.value.get)) {
              x.right = node
            } else x.left = node
            fixColorsAfterInsert(node)
          case node =>
            x = y
            y = if (ord.gt(value, node.value.get)) node.right else node.left
            insert()
        }
      }
    }
  }

  @tailrec
  final def treeMinimum(node: Node): Node = {
    if (node.left == nil)
      node
    else
      treeMinimum(node.left)
  }

  def delete(node: Node): Unit = {
    val doubleColorNode = deleteAndReplaceNode(node)
    doubleColorNode.foreach(fixColorsAfterDelete)
  }

  def deleteAndReplaceNode(node: Node): Option[Node] = {
    if (node.right == nil) {
      val x = node.left
      transplant(node, x)
      if (node.color == NodeColor.Black) Some(x) else None
    } else if (node.left == nil) {
      val x = node.right
      transplant(node, x)
      if (node.color == NodeColor.Black) Some(x) else None
    } else {
      val y = treeMinimum(node.right)
      val x = y.right
      if (y.parent == node)
        x.parent = y
      else {
        transplant(y, y.right)
        y.right = node.right
        y.right.parent = y
      }

      transplant(node, y)
      y.left = node.left
      y.left.parent = y
      val yColor = y.color
      y.color = node.color

      if (yColor == NodeColor.Black) Some(x) else None
    }
  }

  def fixColorsAfterDelete(doubleColorNode: Node): Unit = {
    if (doubleColorNode != root && doubleColorNode.color == NodeColor.Black) {
      if (doubleColorNode == doubleColorNode.parent.right) {
        val sibling = doubleColorNode.parent.left
        if (sibling.color == NodeColor.Red) {
          doubleColorNode.parent.color = NodeColor.Red
          sibling.color = NodeColor.Black
          rightRotate(doubleColorNode.parent)
          fixColorsAfterDelete(doubleColorNode)
        } else {
          if (sibling.left.color == NodeColor.Black && sibling.right.color == NodeColor.Black) {
            sibling.color = NodeColor.Red
            fixColorsAfterDelete(doubleColorNode.parent)
          } else if (sibling.left.color == NodeColor.Black) {
            sibling.right.color = NodeColor.Black
            sibling.color = NodeColor.Red
            leftRotate(sibling)
            fixColorsAfterDelete(doubleColorNode)
          } else {
            sibling.color = sibling.parent.color
            sibling.left.color = NodeColor.Black
            sibling.parent.color = NodeColor.Black
            rightRotate(sibling.parent)
            fixColorsAfterDelete(root)
          }
        }
      } else {
        val sibling = doubleColorNode.parent.right
        if (sibling.color == NodeColor.Red) {
          doubleColorNode.parent.color = NodeColor.Red
          sibling.color = NodeColor.Black
          leftRotate(doubleColorNode.parent)
          fixColorsAfterDelete(doubleColorNode)
        } else {
          if (sibling.left.color == NodeColor.Black && sibling.right.color == NodeColor.Black) {
            sibling.color = NodeColor.Red
            fixColorsAfterDelete(doubleColorNode.parent)
          } else if (sibling.right.color == NodeColor.Black) {
            sibling.left.color = NodeColor.Black
            sibling.color = NodeColor.Red
            rightRotate(sibling)
            fixColorsAfterDelete(doubleColorNode)
          } else {
            sibling.color = sibling.parent.color
            sibling.right.color = NodeColor.Black
            sibling.parent.color = NodeColor.Black
            leftRotate(sibling.parent)
            fixColorsAfterDelete(root)
          }
        }
      }
    } else {
      doubleColorNode.color = NodeColor.Black
    }
  }

  private def transplant(node: Node, replacement: Node): Unit = {
    if (node == root) {
      root = replacement
    } else if (node == node.parent.left)
      node.parent.left = replacement
    else
      node.parent.right = replacement

    replacement.parent = node.parent
  }

  private[datastructures] def blackHeight(node: Node): Int = node match {
    case `nil` => 1
    case Node(_, NodeColor.Red, _, left, right) =>
      blackHeight(left) max blackHeight(right)
    case Node(_, _, _, left, right) =>
      (blackHeight(left) max blackHeight(right)) + 1
  }

  private[datastructures] def rightRotate(node: Node): Unit = {
    if (node.left == nil)
      throw new IllegalStateException()

    val left = node.left
    node.left = left.right

    if (left.right != nil)
      left.right.parent = node

    if (node.parent.left == node)
      node.parent.left = left
    else if (node.parent.right == node)
      node.parent.right = left

    left.parent = node.parent
    left.right = node
    node.parent = left

    if (node == root) {
      root = left
      root.color = NodeColor.Black
    }
  }

  private[datastructures] def leftRotate(node: Node): Unit = {
    if (node.right == nil)
      throw new IllegalStateException()

    val right = node.right
    node.right = right.left

    if (right.left != nil)
      right.left.parent = node

    if (node.parent.left == node)
      node.parent.left = right
    else if (node.parent.right == node)
      node.parent.right = right

    right.parent = node.parent
    right.left = node
    node.parent = right

    if (node == root) {
      root = right
      right.color = NodeColor.Black
    }
  }

  sealed trait NodeColor {}

  private[datastructures] case class Node(value: Option[T],
                                          var color: NodeColor,
                                          var parent: Node,
                                          var left: Node,
                                          var right: Node) {
    override def toString: String = value.getOrElse("empty").toString
  }

  private[datastructures] object NodeColor {

    case object Red extends NodeColor

    case object Black extends NodeColor

  }

}
