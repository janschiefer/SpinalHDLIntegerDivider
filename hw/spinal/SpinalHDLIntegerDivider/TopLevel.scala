package SpinalHDLIntegerDivider

import spinal.core._

// Hardware definition
case class MyTopLevel(num_bit_width: Int) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val divisor = in Bits (num_bit_width bits)
    val dividend = in Bits (num_bit_width bits)
    val quotient = out Bits (num_bit_width bits)
    val remainder = out Bits (num_bit_width bits)
    val busy = out Bool ()
  }

  val divider_module = SpinalHDLIntegerDivider(num_bit_width)

  divider_module.io <> this.io

}

object TopLevelVerilog extends App {
  Config.spinal.generateVerilog(MyTopLevel(num_bit_width = 8))
}

object TopLevelVhdl extends App {
  Config.spinal.generateVhdl(MyTopLevel(num_bit_width = 8))
}
