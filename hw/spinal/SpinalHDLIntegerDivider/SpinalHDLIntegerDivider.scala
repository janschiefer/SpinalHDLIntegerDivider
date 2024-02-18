package SpinalHDLIntegerDivider

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// Hardware definition
case class SpinalHDLIntegerDivider(num_bit_width: Int) extends Component {

  val io = new Bundle {

    val start = in Bool ()
    val divisor = in Bits (num_bit_width bits)
    val dividend = in Bits (num_bit_width bits)
    val quotient = out Bits (num_bit_width bits)
    val remainder = out Bits (num_bit_width bits)
    val busy = out Bool ()

  }

  val dd = RegInit(U(0, num_bit_width bits)) // dividend
  val dr = RegInit(U(0, num_bit_width bits)) // divisor
  val q = RegInit(U(0, num_bit_width bits)) // quotient
  val r = RegInit(U(0, num_bit_width bits)) // remainder
  val bits = RegInit(U(0, (log2Up(num_bit_width) + 1) bits)) // bits

  val busy_reg = RegInit(False) // bits

  io.quotient := q.asBits
  io.remainder := r.asBits
  io.busy := busy_reg

  val fsm = new StateMachine {

    val idle, prepare, shift, sub, done = State()

    setEntry(idle)

    idle.whenIsActive {

      dd := io.dividend.asUInt
      dr := io.divisor.asUInt

      when(io.start) {
        busy_reg := True
        goto(prepare)
      }
    }

    prepare.whenIsActive {

      q := 0
      r := 0
      bits := U(num_bit_width)

      when(dr === U(0).resized) { // division by zero
        q := (default -> true)
        r := (default -> true)
        goto(done)
      }
        .elsewhen(dr > dd) { // divisor > dididend
          r := dd
          goto(done)
        }
        .elsewhen(dr === dd) { // divisor == dididend
          q := U(1).resized
          goto(done)
        }
        .otherwise {
          goto(shift)
        }
    }

    shift.whenIsActive {
      val tmp = r((num_bit_width - 2) downto 0) ## dd(num_bit_width - 1)
      when(tmp.asUInt < dr) {
        bits := bits - U(1).resized;
        r := tmp.asUInt
        dd := (dd((num_bit_width - 2) downto 0) ## False).asUInt
      }
        .otherwise {
          goto(sub)
        }

    }

    sub.whenIsActive {

      when(bits > U(0).resized) {
        val tmp_remainder =
          (r((num_bit_width - 2) downto 0) ## dd(num_bit_width - 1)).asUInt
        r := tmp_remainder
        dd := (dd((num_bit_width - 2) downto 0) ## False).asUInt
        // remainder minus divisor
        val diff = tmp_remainder - dr
        when(diff(num_bit_width - 1) === False) { // No underflow
          q := (q(
            (num_bit_width - 2) downto 0
          ) ## True).asUInt // slide 1 into result
          r := diff
        }
          .otherwise { // Underflow
            q := (q(
              (num_bit_width - 2) downto 0
            ) ## False).asUInt // slide 0 in and continue to claculate with old value
          }
        bits := bits - U(1).resized;
      }
        .otherwise {
          goto(done)
        }

    }

    done.whenIsActive {

      busy_reg := False
      when(io.start === False) {
        goto(idle)
      }

    }

  }

}

object SpinalHDLIntegerDivider {}
