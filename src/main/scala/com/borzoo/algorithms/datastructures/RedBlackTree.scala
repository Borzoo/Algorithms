package com.borzoo.algorithms.datastructures

import scala.annotation.tailrec

class RedBlackTree[T](implicit ord: Ordering[T]) {

    private[datastructures] val nil: Node = Node(None, NodeColor.Black, None, None, None)
    private[datastructures] var root: Node = nil

    def all(): Seq[T] = root match {
        case value if value == nil => Seq.empty
        case _ => walk(root)
    }

    private def walk(root: Node): List[T] = {
        def walk(node: Node, acc: List[T]): List[T] = {
            var acc2 = acc

            acc2 = node.left.map {
                case `nil` => acc2
                case l => walk(l, acc2)
            }.get

            acc2 = node.right.map {
                case `nil` => acc2
                case r => walk(r, acc2)
            }.get

            node.value.get +: acc2
        }

        walk(root, List())
    }

    def insert(value: T): Unit = {
        if (root == nil) {
            root = Node(Some(value), NodeColor.Black, Some(nil), Some(nil), Some(nil))
        }
        else {
            var x = root
            var y = if (ord.gt(value, root.value.get)) root.right else root.left
            insert()

            @tailrec
            def insert(): Unit = {
                y match {
                    case Some(`nil`) =>
                        val node = Node(Some(value), NodeColor.Black, Some(x), Some(nil), Some(nil))
                        if (ord.gt(value,  x.value.get)) {
                            x.right = Some(node)
                        }
                        else x.left = Some(node)
                    case Some(node) =>
                        x = y.get
                        y = if (ord.gt(value, node.value.get)) node.right else node.left
                        insert()
                }
            }
        }
    }

    sealed trait NodeColor {
    }

    case class Node(value: Option[T]
                    , color: NodeColor
                    , var parent: Option[Node]
                    , var left: Option[Node]
                    , var right: Option[Node])

    object NodeColor {

        case object Red extends NodeColor

        case object Black extends NodeColor

    }

}
