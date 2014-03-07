package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig || a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val ia1, ia2, outAnd = new Wire
    inverter(a1, ia1)
    inverter(a2, ia2)
    andGate(ia1, ia2, outAnd)
    inverter(outAnd, output)
  }

  def basicDemux(in: Wire, c: Wire, out1: Wire, out0: Wire) {
    val cinv = new Wire
    inverter(c, cinv)
    andGate(in, cinv, out0)
    andGate(in, c, out1)
  }

  def mirror(in: Wire, out: Wire){
    val inv = new Wire
    inverter(in, inv)
    inverter(inv, out)
  }

  def demux(in: Wire, controls: List[Wire], out: List[Wire]) {
    if(controls.isEmpty){
      mirror(in, out.head)
    }
    else{
      val c = controls.head
      if(controls.tail.isEmpty){
        basicDemux(in, c, out.head, out.tail.head)
      }
      else{
        val out0 = new Wire
        val out1 = new Wire
        basicDemux(in, c, out1, out0)
        demux(out0, controls.tail, out.drop(out.size/2))
        demux(out1, controls.tail, out.take(out.size/2))
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }
}

object CircuitMain extends App {
  Circuit.andGateExample
}
