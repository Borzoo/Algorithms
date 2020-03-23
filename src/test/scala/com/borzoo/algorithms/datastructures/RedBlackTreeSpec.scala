package com.borzoo.algorithms.datastructures

import org.scalatest.Inside
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RedBlackTreeSpec extends AnyFunSpec with Matchers with Inside {
    describe("a red black tree") {

        def checkTreeProperties(tree: RedBlackTree[Int]): Unit = {
            tree.root.color should be(tree.NodeColor.Black)
            tree.nil.color should be(tree.NodeColor.Black)
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
                    List(1),
                    List(1, 2),
                    List(2, 1),
                    List(2, 1, 3),
                    List(5, 7, 4, 1, -5, 2, 9, 100, 20, 1, 50, 2, 7, 8, -8, 0, 10, 15, 2, 1),
                    List(11, 2, 14, 15, 1, 7, 5, 8, 4),
                    3 to 1 by -1,
                    5 to 1 by -1,
                    100 to 1 by -1,
                    1 to 3,
                    1 to 5,
                    1 to 100,
                    List.fill(100)(5)
                )) {
                    val rbt = new RedBlackTree[Int]()
                    testCase.foreach(rbt.insert)

                    checkTreeProperties(rbt)
                    rbt.root.parent should be(rbt.nil)
                    rbt.all should equal(testCase.sorted)
                }
            }
        }
        
        describe("after deleting nodes"){
            it("should have a nil root if the only node is deleted"){
                val rbt = new RedBlackTree[Int]()
                rbt.insert(1)
                
                rbt.delete(rbt.root)
                
                rbt.root should be(rbt.nil)
                rbt.all() should equal(List())
            }
            
            it("should not violate tree properties after deleting a node"){
                for (testCase <- List(
                    List(List(1, 2), List(2)),
                    List(List(1, 2), List(1)),
                    List(List(2, 1), List(1)),
                    List(List(2, 1), List(2)),
                    List(List(2, 1), List(2, 1)),
                    List(List(2, 1), List(1, 2)),
                    List(1 to 100, 70 to 20 by -1),
                    List(100 to 1 by -1, 20 to 70)
                );
                    itemsToInsert = testCase.head;
                    itemsToDelete <- testCase.tail
                     ) {
                    
                    val rbt = new RedBlackTree[Int]()
                    itemsToInsert.foreach(rbt.insert)

                    itemsToDelete.map(itemToDelete => rbt.allNodes().filter(_.value == Some(itemToDelete)).head).foreach(rbt.delete)
                    
                    checkTreeProperties(rbt)
                    rbt.root.parent should be(rbt.nil)
                    rbt.all should equal(itemsToInsert.diff(itemsToDelete).sorted)
                }
            }
        }
        
        it("should be able to find the minimum"){
            for(testCase <- List(
                (List(1), 1),
                (List(1, 2), 1),
                (List(2, 1), 1),
                (List(3, 4, 2, 1), 1),
                (List(3, 4, 2, 3), 2),
                (List(1, 6, 5, 3, 2), 1)
            )){
                val rbt = new RedBlackTree[Int]()
                testCase._1.foreach(rbt.insert)
                
                rbt.treeMinimum(rbt.root).value should be(Some(testCase._2))
            }
        }
    }
}
