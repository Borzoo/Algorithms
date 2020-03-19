package com.borzoo.algorithms.datastructures

import scala.annotation.tailrec

class RedBlackTree[T](implicit ord: Ordering[T]) {
    private[datastructures] val nil: Node = Node(None, NodeColor.Black, null, null, null)
    private[datastructures] var root: Node = nil

    def all(): Seq[T] = root match {
        case value if value == nil => Seq.empty
        case _ => walk(root)
    }

    private def walk(root: Node): List[T] = {
        def walk(node: Node, acc: List[T]): List[T] = {
            var acc2 = acc

            acc2 = node.left match {
                case `nil` => acc2
                case l => walk(l, acc2)
            }


            acc2 = acc2 :+ node.value.get

            acc2 = node.right match {
                case `nil` => acc2
                case r => walk(r, acc2)
            }
            
            acc2
        }

        walk(root, List())
    }

    @tailrec
    final def fixColors(node: Node): Unit = {
        if (node == root)
            node.color = NodeColor.Black

        if (node.parent.color == NodeColor.Red) {
            if (node.parent == node.parent.parent.left) {
                val uncle = node.parent.parent.right
                if (uncle.color == NodeColor.Red) {
                    node.parent.parent.color = NodeColor.Red
                    uncle.color = NodeColor.Black
                    node.parent.color = NodeColor.Black
                    fixColors(node.parent.parent)
                }
                else {
                    if (node == node.parent.right) {
                        leftRotate(node.parent)
                        fixColors(node.left)
                    }
                    else {
                        node.parent.parent.color = NodeColor.Red
                        rightRotate(node.parent.parent)
                        node.parent.color = NodeColor.Black
                        node.color = NodeColor.Red
                        fixColors(root)
                    }
                }
            }
            else {
                val uncle = node.parent.parent.left
                if (uncle.color == NodeColor.Red) {
                    node.parent.parent.color = NodeColor.Red
                    uncle.color = NodeColor.Black
                    node.parent.color = NodeColor.Black
                    fixColors(node.parent.parent)
                }
                else {
                    if (node == node.parent.left) {
                        rightRotate(node.parent)
                        fixColors(node.right)
                    }
                    else {
                        node.parent.parent.color = NodeColor.Red
                        leftRotate(node.parent.parent)
                        node.parent.color = NodeColor.Black
                        node.color = NodeColor.Red
                        fixColors(root)
                    }
                }
            }
        }
    }

    def insert(value: T): Unit = {
        if (root == nil) {
            root = Node(Some(value), NodeColor.Black, nil, nil, nil)
        }
        else {
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
                        }
                        else x.left = node
                        fixColors(node)
                    case node =>
                        x = y
                        y = if (ord.gt(value, node.value.get)) node.right else node.left
                        insert()
                }
            }
        }
    }

    private[datastructures] def blackHeight(node: Node): Int = node match {
        case `nil` => 1
        case Node(_, NodeColor.Red, _, left, right) => blackHeight(left) max blackHeight(right)
        case Node(_, _, _, left, right) => (blackHeight(left) max blackHeight(right)) + 1
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

    sealed trait NodeColor {
    }

    private[datastructures] case class Node(value: Option[T]
                                            , var color: NodeColor
                                            , var parent: Node
                                            , var left: Node
                                            , var right: Node) {
        override def toString: String = value.getOrElse("empty").toString
    }

    private[datastructures] object NodeColor {

        case object Red extends NodeColor

        case object Black extends NodeColor

    }

}
