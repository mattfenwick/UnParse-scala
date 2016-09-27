package com.unparse_scala

import org.scalatest.{FunSpecLike, Matchers}

/**
  * Created by matt.fenwick on 9/27/16.
  */
class MaybeErrorSpec extends FunSpecLike with Matchers {
  describe("MaybeError") {
    val inc = (x: Int) => x + 1
    val f_b = (x: Any) => x match {
      case 3 => Success(4)
      case 4 => Zero
      case _ => Error("e1")
    }
    def f_a[A, B, C](x: A, y: B, z: C): (A, C, B, C) = (x, z, y, z)
    val g1 = Success("g1")
    val g2 = Success("g2")
    val g3 = Success(3)
    val g4 = Success(4)
    val e1 = Error("e1")
    val e2 = Error("e2")
    val e3 = Error(3)
    val e4 = Error(4)
    val b1 = Zero

    describe("equality") {
      it("should check by value, not reference") {
        g1 should equal(Success("g1"))
        b1 should equal(Zero)
        e1 should equal(Error("e1"))
      }
    }

    describe("fmap") {
      it("should change success, leave zero and error unchanged") {
        g3.fmap(inc) should equal(g4)
        b1.fmap(inc) should equal(b1)
        e3.fmap(inc) should equal(e3)
      }
    }

    describe("bind") {
      it("should change success, leave zero and error unchanged") {
        g2.bind(f_b) should equal(e1)
        g2.bind(f_b) should equal(e1)
        g3.bind(f_b) should equal(g4)
        g4.bind(f_b) should equal(b1)
        b1.bind(f_b) should equal(b1)
        e1.bind(f_b) should equal(e1)
      }
    }

    describe("app") {
      it("should get short-circuited if there's any zeroes or errors") {
        // apply over lots of success
        MaybeError.app3(f_a[String, String, Int], g1, g2, g3) should equal(Success(("g1", 3, "g2", 3)))
        // short-circuit zero
        MaybeError.app3(f_a[String, Any, String], g1, b1, g2) should equal(b1)
        // short-circuit error
        MaybeError.app3(f_a[String, Int, Any], g1, g3, e1) should equal(e1)
      }
    }

    describe("plus") {
      it("should say that good + anything = good") {
        // good + x -> good (left-biased)
        g1.plus(g2) should equal(g1)
        g1.plus(b1) should equal(g1)
        g1.plus(e1) should equal(g1)
      }
      it("should say that bad + anything = anything") {
        // bad + x -> x
        b1.plus(g1) should equal(g1)
        b1.plus(b1) should equal(b1)
        b1.plus(e1) should equal(e1)
      }
      it("should say that error + anything = error") {
        // error + x -> error (left-biased)
        e1.plus(g1) should equal(e1)
        e1.plus(b1) should equal(e1)
        e1.plus(e2) should equal(e1)
      }
    }

  }
}
