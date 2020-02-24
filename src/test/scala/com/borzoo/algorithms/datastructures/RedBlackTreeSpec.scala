package com.borzoo.algorithms.datastructures

import org.scalatest.Inspectors._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RedBlackTreeSpec extends AnyFunSpec with Matchers {
    describe("a red black tree") {
        val testCases = List(
            List(1),
            List(1, 2, 3, 4, 5),
            List(5, 4, 3, 2, 1),
            List(3, 5, 4, 1, 2)
        )

        describe("when getting all") {
            it("should return an empty list if it has no node") {
                val rbt = new RedBlackTree[Int]()
                rbt.all should be(empty)
            }

            it("should return a list containing all items if it has any node") {
                forEvery(testCases)(testCase => {
                    val rbt = new RedBlackTree[Int]
                    testCase.foreach(rbt.insert)

                    rbt.all should contain theSameElementsAs testCase
                })
            }
        }

        describe("when not empty"){
            it("should have a black root with a nil parent"){
                forEvery(testCases)(testCase => {
                    val rbt = new RedBlackTree[Int]()
                    testCase.foreach(rbt.insert)
                    rbt.root should matchPattern { case rbt.Node(Some(_), rbt.NodeColor.Black, rbt.nil, _, _) => }
                })
            }
        }

        describe("when a node is left-rotated"){
            it("should throw an error if its right child is nil"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                rbt.root.left = rbt.Node(Some(2), rbt.NodeColor.Black, rbt.root, rbt.nil, rbt.nil)

                an [IllegalStateException] should be thrownBy rbt.leftRotate(rbt.root)
            }

           it("should set the right child as root if root is rotated"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                rbt.root.right = rbt.Node(Some(2), rbt.NodeColor.Black, rbt.root, rbt.nil, rbt.nil)

                rbt.leftRotate(rbt.root)

                rbt.root should matchPattern { case rbt.Node(Some(2), _, rbt.nil, _, _) => }
            }

            it("should rotate node correctly"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)

                val root = rbt.root
                val right = rbt.Node(Some(2), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)
                root.right = right
                val rightLeft = rbt.Node(Some(3), rbt.NodeColor.Black, right, rbt.nil, rbt.nil)
                right.left = rightLeft
                val rootLeft = rbt.Node(Some(3), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)
                root.left = rootLeft
                val rightRight = right.right
                val nil = rbt.nil

                rbt.leftRotate(rbt.root)

                rbt.root should matchPattern { case rbt.Node(Some(2), _, `nil`, `root`, `rightRight`) => }
                root should matchPattern { case rbt.Node(Some(1), _, `right`, `rootLeft`, `rightLeft` ) => }
            }
        }
    }
}
