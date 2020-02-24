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

        describe("when a node is right-rotated"){
            it("should throw an error if its left child is nil"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                rbt.root.right = rbt.Node(Some(2), rbt.NodeColor.Black, rbt.root, rbt.nil, rbt.nil)

                an [IllegalStateException] should be thrownBy rbt.rightRotate(rbt.root)
            }

            it("should set the left child as root if root is rotated"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                rbt.root.left = rbt.Node(Some(2), rbt.NodeColor.Black, rbt.root, rbt.nil, rbt.nil)

                rbt.rightRotate(rbt.root)

                rbt.root should matchPattern { case rbt.Node(Some(2), _, rbt.nil, _, _) => }
            }

            it("should rotate node correctly"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)

                val root = rbt.root
                val left = rbt.Node(Some(2), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)
                root.left = left
                val leftRight = rbt.Node(Some(3), rbt.NodeColor.Black, left, rbt.nil, rbt.nil)
                left.right = leftRight
                val rootRight = rbt.Node(Some(3), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)
                root.right = rootRight
                val leftLeft = left.left
                val nil = rbt.nil

                rbt.rightRotate(rbt.root)

                rbt.root should matchPattern { case rbt.Node(Some(2), _, `nil`, `leftLeft`, `root`) => }
                root should matchPattern { case rbt.Node(Some(1), _, `left`, `leftRight`, `rootRight`) => }
            }
        }

        describe("when a node is rotated twice"){
            it("should be reset if rotated left and then right"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)

                val root = rbt.root
                root.left = rbt.Node(Some(2), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)
                root.left.right = rbt.Node(Some(3), rbt.NodeColor.Black, root.left, rbt.nil, rbt.nil)
                root.right = rbt.Node(Some(3), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)

                rbt.rightRotate(rbt.root)
                rbt.leftRotate(rbt.root)

                rbt.root should equal (root)
                rbt.root.left should equal(root.left)
                rbt.root.right should equal(root.right)
            }

            it("should be reset if rotated right and then left"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)

                val root = rbt.root
                root.left = rbt.Node(Some(2), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)
                root.left.right = rbt.Node(Some(3), rbt.NodeColor.Black, root.left, rbt.nil, rbt.nil)
                root.right = rbt.Node(Some(3), rbt.NodeColor.Black, root, rbt.nil, rbt.nil)

                rbt.leftRotate(rbt.root)
                rbt.rightRotate(rbt.root)

                rbt.root should equal (root)
                rbt.root.left should equal(root.left)
                rbt.root.right should equal(root.right)
            }
        }

        describe("when a node is inserted"){
            it("should be black when the node is root"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                rbt.insert(2)

                rbt.root should matchPattern { case rbt.Node(Some(1), rbt.NodeColor.Black, rbt.nil, rbt.nil, rbt.nil) => }
            }

            it("should be red when its parent is black"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(2)
                rbt.insert(1)
                rbt.insert(3)
                val root = rbt.root;

                rbt.root.right should matchPattern { case rbt.Node(Some(3), rbt.NodeColor.Red, `root`, rbt.nil, rbt.nil) => }
                rbt.root.left should matchPattern { case rbt.Node(Some(1), rbt.NodeColor.Red, `root`, rbt.nil, rbt.nil) => }
            }
        }
    }
}
