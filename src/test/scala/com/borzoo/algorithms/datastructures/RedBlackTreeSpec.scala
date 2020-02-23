package com.borzoo.algorithms.datastructures

import org.scalatest.Inspectors._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RedBlackTreeSpec extends AnyFunSpec with Matchers {
    describe("a red black tree") {
        describe("when getting all") {
            it("should return an empty list if it has no node") {
                val rbt = new RedBlackTree[Int]()
                rbt.all should be(empty)
            }

            it("should return a list containing all items if it has any node") {
                forEvery(List(
                    List(1),
                    List(1, 2, 3, 4, 5),
                    List(5, 4, 3, 2, 1),
                    List(3, 5, 4, 1, 2)
                ))(testCase => {
                    val rbt = new RedBlackTree[Int]
                    testCase.foreach(rbt.insert)

                    rbt.all should contain theSameElementsAs testCase
                })
            }
        }
    }
}
