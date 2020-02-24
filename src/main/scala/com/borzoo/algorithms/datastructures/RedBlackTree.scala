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

            acc2 = node.right match {
                case `nil` => acc2
                case r => walk(r, acc2)
            }

            node.value.get +: acc2
        }

        walk(root, List())
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
                        val node = Node(Some(value), NodeColor.Black, x, nil, nil)
                        if (ord.gt(value,  x.value.get)) {
                            x.right = node
                        }
                        else x.left = node
                    case node =>
                        x = y
                        y = if (ord.gt(value, node.value.get)) node.right else node.left
                        insert()
                }
            }
        }
    }

    private[datastructures] def rightRotate(node: Node): Unit = {
        if(node.left == nil)
            throw new IllegalStateException()

        val left = node.left
        node.left = left.right

        if(left.right != nil)
            left.right.parent = node

        left.parent = node.parent
        left.right = node
        node.parent = left

        if(node == root) {
            root = left
        }
    }

    private[datastructures] def leftRotate(node: Node): Unit = {
        if(node.right == nil)
            throw new IllegalStateException()

        val right = node.right
        node.right = right.left

        if(right.left != nil)
            right.left.parent = node

        right.parent = node.parent
        right.left = node
        node.parent = right

        if(node == root) {
            root = right
        }
    }

    sealed trait NodeColor {
    }

    private[datastructures] case class Node(value: Option[T]
                    , var color: NodeColor
                    , var parent: Node
                    , var left: Node
                    , var right: Node){
        override def toString: String = value.getOrElse("empty").toString
    }

    private[datastructures] object NodeColor {

        case object Red extends NodeColor

        case object Black extends NodeColor

    }

}
