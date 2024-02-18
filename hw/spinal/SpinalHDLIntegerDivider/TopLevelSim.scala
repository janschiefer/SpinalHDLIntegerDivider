package SpinalHDLIntegerDivider

import spinal.core._
import spinal.core.sim._

object MyTopLevelSim extends App {
  Config.sim.compile(MyTopLevel(num_bit_width = 8)).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    for (idx2 <- 0 to 255) {
      for (idx <- 0 to 255) {
        // Drive the dut inputs with random values
        dut.io.dividend #= idx
        dut.io.divisor #= idx2
        dut.io.start #= false

        // Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        dut.io.start #= true

        // Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        // Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        waitUntil(dut.io.busy.toBoolean == false)

        dut.clockDomain.waitRisingEdge()

        if (dut.io.divisor.toInt != 0) {

          assert(
            (dut.io.dividend.toInt / dut.io.divisor.toInt) == dut.io.quotient.toInt
          )

          assert(
            (dut.io.dividend.toInt % dut.io.divisor.toInt) == dut.io.remainder.toInt
          )

        }

        dut.io.start #= false

      }
    }

  }
}
