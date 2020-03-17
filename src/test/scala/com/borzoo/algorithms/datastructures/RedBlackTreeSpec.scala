package com.borzoo.algorithms.datastructures

import org.scalatest.Inside
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RedBlackTreeSpec extends AnyFunSpec with Matchers with Inside {
    describe("a red black tree") {

        def checkTreeProperties(tree: RedBlackTree[Int]): Unit = {
            tree.root.color should be(tree.NodeColor.Black)
            checkTreeNodeProperties(tree.root)

            def checkTreeNodeProperties(treeNode: tree.Node): Unit = {

                if (treeNode.color == tree.NodeColor.Red) {
                    treeNode.left.color should be(tree.NodeColor.Black)
                    treeNode.right.color should be(tree.NodeColor.Black)
                }

                if (treeNode != tree.nil) {
                    tree.blackHeight(treeNode.left) should be(tree.blackHeight(treeNode.right))

                    if (treeNode.left != tree.nil) {
                        treeNode.value.map(_ should be >= treeNode.left.value.get)
                        checkTreeNodeProperties(treeNode.left)
                    }

                    if (treeNode.right != tree.nil) {
                        treeNode.value.map(_ should be <= treeNode.right.value.get)
                        checkTreeNodeProperties(treeNode.right)
                    }
                }
            }
        }

        describe("an empty tree") {
            it("should not violate tree properties") {
                val rbt = new RedBlackTree[Int]()

                checkTreeProperties(rbt)
                rbt.root should be(rbt.nil)
                rbt.all should equal(List())
            }
        }

        describe("after inserting nodes") {

            it("should not violate tree properties after inserting items") {
                for (testCase <- List(
                    List(2, 1, 3))) {
                    val rbt = new RedBlackTree[Int]()
                    testCase.foreach(rbt.insert)

                    checkTreeProperties(rbt)
                    rbt.root.parent should be(rbt.nil)
                    rbt.all should equal(testCase.sorted)
                }
            }
        }
    }
}
