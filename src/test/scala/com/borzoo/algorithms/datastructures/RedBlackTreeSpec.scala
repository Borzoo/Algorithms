package com.borzoo.algorithms.datastructures

import org.scalatest.Inside
import org.scalatest.Inspectors._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RedBlackTreeSpec extends AnyFunSpec with Matchers with Inside {
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

        describe("when not empty") {
            it("should have a black root with a nil parent") {
                forEvery(testCases)(testCase => {
                    val rbt = new RedBlackTree[Int]()
                    testCase.foreach(rbt.insert)
                    rbt.root should matchPattern { case rbt.Node(Some(_), rbt.NodeColor.Black, rbt.nil, _, _) => }
                })
            }
        }

        describe("when a node is left-rotated") {
            it("should throw an error if its right child is nil") {
                val rbt = new RedBlackTree[Int]()
                rbt.insert(2)
                rbt.insert(1)

                an[IllegalStateException] should be thrownBy rbt.leftRotate(rbt.root)
            }

            it("should rotate the node correctly") {
                val rbt = new RedBlackTree[Int]()
                rbt.insert(3)
                rbt.insert(2)
                rbt.insert(5)
                rbt.insert(4)
                rbt.insert(6)

                rbt.leftRotate(rbt.root)

                inside(rbt.root) { case rbt.Node(Some(5), rootColor, p, l, r) =>
                    rootColor should be(rbt.NodeColor.Black)
                    p should be(rbt.nil)
                    inside(l) { case rbt.Node(Some(3), _, lp, ll, lr) =>
                        lp should be(rbt.root)
                        inside(lr) { case rbt.Node(Some(4), _, lrp, lrl, lrr) =>
                            lrp should be(l)
                            lrl should be(rbt.nil)
                            lrr should be(rbt.nil)
                        }
                        inside(ll) { case rbt.Node(Some(2), _, llp, lll, llr) =>
                            llp should be(l)
                            lll should be(rbt.nil)
                            llr should be(rbt.nil)
                        }
                    }
                    inside(r) { case rbt.Node(Some(6), _, rp, rl, rr) =>
                        rp should be(rbt.root)
                        rl should be(rbt.nil)
                        rr should be(rbt.nil)
                    }
                }
            }
        }

        describe("when a node is right-rotated") {
            it("should throw an error if its left child is nil") {
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                rbt.root.right = rbt.Node(Some(2), rbt.NodeColor.Black, rbt.root, rbt.nil, rbt.nil)

                an[IllegalStateException] should be thrownBy rbt.rightRotate(rbt.root)
            }

            it("should rotate the node correctly") {
                val rbt = new RedBlackTree[Int]()
                rbt.insert(5)
                rbt.insert(6)
                rbt.insert(3)
                rbt.insert(2)
                rbt.insert(4)

                rbt.rightRotate(rbt.root)

                inside(rbt.root) { case rbt.Node(Some(3), rootColor, p, l, r) =>
                    rootColor should be(rbt.NodeColor.Black)
                    p should be(rbt.nil)
                    inside(r) { case rbt.Node(Some(5), _, rp, rl, rr) =>
                        rp should be(rbt.root)
                        inside(rr) { case rbt.Node(Some(6), _, rrp, rrl, rrr) =>
                            rrp should be(r)
                            rrl should be(rbt.nil)
                            rrr should be(rbt.nil)
                        }
                        inside(rl) { case rbt.Node(Some(4), _, rlp, rll, rlr) =>
                            rlp should be(r)
                            rll should be(rbt.nil)
                            rlr should be(rbt.nil)
                        }
                    }
                    inside(l) { case rbt.Node(Some(2), _, lp, ll, lr) =>
                        lp should be(rbt.root)
                        ll should be(rbt.nil)
                        lr should be(rbt.nil)
                    }
                }
            }
        }

        describe("left and right rotate should cancel each other") {
            def createTree = {
                val rbt = new RedBlackTree[Int]()
                rbt.insert(3)
                rbt.insert(1)
                rbt.insert(2)
                rbt.insert(5)
                rbt.insert(6)
                rbt
            }

            def assertTree(rbt: RedBlackTree[Int]) = {
                inside(rbt.root) { case rbt.Node(Some(3), _, p, l, r) =>
                    p should be(rbt.nil)
                    inside(l) { case rbt.Node(Some(1), _, lp, ll, lr) =>
                        lp should be(rbt.root)
                        ll should be(rbt.nil)
                        inside(lr) { case rbt.Node(Some(2), _, lrp, lrl, lrr) =>
                            lrp should be(l)
                            lrl should be(rbt.nil)
                            lrr should be(rbt.nil)
                        }
                    }
                    inside(r) { case rbt.Node(Some(5), _, rp, rl, rr) =>
                        rp should be(rbt.root)
                        rl should be(rbt.nil)
                        inside(rr) { case rbt.Node(Some(6), _, rrp, rrl, rrr) =>
                            rrp should be(r)
                            rrl should be(rbt.nil)
                            rrr should be(rbt.nil)
                        }
                    }
                }
            }

            it("should be reset if rotated left and then right") {
                val rbt = createTree
                
                rbt.rightRotate(rbt.root)
                rbt.leftRotate(rbt.root)
                
                assertTree(rbt)
            }

            it("should be reset if rotated right and then left") {
                val rbt = createTree
                
                rbt.leftRotate(rbt.root)
                rbt.rightRotate(rbt.root)
                
                assertTree(rbt)
            }
            
            it("rotations should be commutative"){
                val rbt = createTree
                
                rbt.leftRotate(rbt.root)
                rbt.rightRotate(rbt.root)
                rbt.leftRotate(rbt.root)
                rbt.leftRotate(rbt.root)
                rbt.rightRotate(rbt.root)
                rbt.rightRotate(rbt.root)
                
                assertTree(rbt)
            }
        }

        describe("when a node is inserted") {
            it("should be black when the node is root") {
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                rbt.insert(2)

                rbt.root should matchPattern { case rbt.Node(Some(1), rbt.NodeColor.Black, _, _, _) => }
            }

            it("should be red when its parent is black") {
                val rbt = new RedBlackTree[Int]()
                rbt.insert(2)
                rbt.insert(1)
                rbt.insert(3)
                val root = rbt.root

                rbt.root.right should matchPattern { case rbt.Node(Some(3), rbt.NodeColor.Red, `root`, rbt.nil, rbt.nil) => }
                rbt.root.left should matchPattern { case rbt.Node(Some(1), rbt.NodeColor.Red, `root`, rbt.nil, rbt.nil) => }
            }
        }
    }
}
