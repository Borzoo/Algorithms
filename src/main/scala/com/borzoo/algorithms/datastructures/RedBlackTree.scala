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

    sealed trait NodeColor {
    }

    case class Node(value: Option[T]
                    , color: NodeColor
                    , var parent: Node
                    , var left: Node
                    , var right: Node)

    object NodeColor {

        case object Red extends NodeColor

        case object Black extends NodeColor

    }

}
