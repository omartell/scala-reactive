package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("demux example 1") {
    val in1  = new Wire
    val out1 = new Wire

    in1.setSignal(true)

    demux(in1, List(), List(out1))

    run

    assert(out1.getSignal == true, "demux 1")

    in1.setSignal(false)

    run

    assert(out1.getSignal == false, "demux 1")
  }

  test("demux example 2") {
    val in1  = new Wire
    val c0  = new Wire
    val out0 = new Wire
    val out1 = new Wire

    in1.setSignal(true)

    c0.setSignal(true)

    demux(in1, List(c0), List(out0, out1).reverse)

    run

    assert(out0.getSignal == false, "demux 2")
    assert(out1.getSignal == true, "demux 2")
  }

  test("demux example 3") {
    val in1 = new Wire
    val c0  = new Wire
    val c1  = new Wire
    val out0 = new Wire
    val out1 = new Wire
    val out2 = new Wire
    val out3 = new Wire

    in1.setSignal(true)

    c0.setSignal(false)
    c1.setSignal(false)

    demux(in1, List(c0, c1).reverse, List(out0, out1, out2, out3).reverse)

    run

    c0.setSignal(true)
    c1.setSignal(true)

    run

    assert(out0.getSignal == false, "demux 3")
    assert(out1.getSignal == false, "demux 3")
    assert(out2.getSignal == false, "demux 3")
    assert(out3.getSignal == true, "demux 3")
  }

  test("demux example 4") {
    val in1 = new Wire
    val c0  = new Wire
    val c1  = new Wire
    val c2  = new Wire
    val c3  = new Wire
    val out0 = new Wire
    val out1 = new Wire
    val out2 = new Wire
    val out3 = new Wire
    val out4 = new Wire
    val out5 = new Wire
    val out6 = new Wire
    val out7 = new Wire
    val out8 = new Wire
    val out9 = new Wire
    val out10 = new Wire
    val out11 = new Wire
    val out12 = new Wire
    val out13 = new Wire
    val out14 = new Wire
    val out15 = new Wire

    in1.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    c3.setSignal(false)

    val outs = List(out0, out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15).reverse

    demux(in1, List(c0, c1, c2,c3).reverse, outs)

    run

    c0.setSignal(true)
    c1.setSignal(true)
    c2.setSignal(true)
    c3.setSignal(true)

    run

    assert(out0.getSignal == false, "demux 4")
    assert(out1.getSignal == false, "demux 4")
    assert(out2.getSignal == false, "demux 4")
    assert(out15.getSignal == true, "demux 4")
  }

  test("altOrGate example") {
    val in1, in2, out = new Wire

    orGate2(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)

    run

    assert(out.getSignal == false, "altGate 1")


    in1.setSignal(true)

    run

    assert(out.getSignal == true, "altGate 1")

    in2.setSignal(true)

    run

    assert(out.getSignal == true, "altGate 1")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

}
