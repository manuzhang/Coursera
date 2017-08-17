package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 1")

    in1.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 3")
    
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate example") {
    val in1, in2, out = new Wire
    
    orGate2(in1, in2, out)
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

  test("demux example") {
    val iw, ow = new Wire
    demux(iw, List(), List(ow))

    iw.setSignal(false)
    run
    assert(ow.getSignal === false, "o false")

    iw.setSignal(true)
    run
    assert(ow.getSignal === true, "o true")

    val in = new Wire
    val c1, c2, c3, c4 = new Wire
    val o1, o2, o3, o4,
        o5, o6, o7, o8,
        o9, o10, o11, o12,
        o13, o14, o15, o16
        = new Wire
    val conList = List(c1, c2, c3, c4); 
    val outList = List(o1, o2, o3, o4,
        o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16); 
    demux(in, conList, outList);
    in.setSignal(false)
    c1.setSignal(true)
    c2.setSignal(true)
    c3.setSignal(true)
    c4.setSignal(true)
    run

    for (o <- outList) {
     assert(o.getSignal === false, "demux all false") 
    }
    
    
    in.setSignal(true)
    run
    assert(o16.getSignal === true, "demux o16 true")
    
   
    c1.setSignal(false)
    run
    assert(o8.getSignal === true, "demux o8 true")
    
    c2.setSignal(false)
    run
    assert(o4.getSignal === true, "demux o4 true")
    
    c3.setSignal(false)
    run
    assert(o2.getSignal === true, "demux o2 true")
    	
    c4.setSignal(false)
    run
    assert(o1.getSignal === true, "demux o1 true")
        

  }
}
