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
      () =>
        afterDelay(0) {
          println(
            //"  " + currentTime + ": " + name + " -> " +  wire.getSignal)
            s" $currentTime: $name -> ${wire.getSignal}")
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

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val na1, na2, b = new Wire
      inverter(a1, na1)
      inverter(a2, na2)
      andGate(na1, na2, b)
      afterDelay(OrGateDelay) { output.setSignal(!b.getSignal) }
    }
    a1 addAction orAction
    a2 addAction orAction

  }

  def isGate(in: Wire, output: Wire): Unit = {
    def isAction() {
      afterDelay(0) { output.setSignal(in.getSignal) }
    }
    in addAction isAction
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]): Unit = {
    def setValue(cc: List[Wire], carry: Wire, o: Wire, n: Int): Unit = {
      if (cc.isEmpty) andGate(in, carry, o)
      else {
        val result = new Wire
        if (n % 2 == 1) andGate(cc.head, carry, result)
        else {
          val tmp = new Wire
          inverter(cc.head, tmp)
          andGate(tmp, carry, result)
        }
        setValue(cc.tail, result, o, n / 2)
      }
    }
    if (c.isEmpty) isGate(in, out.head)
    else {
      val indexedOut = out.zipWithIndex
      val reversed = c.reverse
      indexedOut.foreach {
        (tuple) =>
          val wire = tuple._1
          val index = tuple._2
          val carry = new Wire
          carry.setSignal(true)
          setValue(reversed, carry, wire, index)
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

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
