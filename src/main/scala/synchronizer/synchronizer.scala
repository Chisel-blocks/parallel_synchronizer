
// Dsp-block synchronizer
// Description here
// Inititally written by dsp-blocks initmodule.sh, 20201019
// Last modified by Miikka Tenhunen 19.10.2020
package synchronizer

import chisel3.experimental._
import chisel3._
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex
import chisel3.util._

class synchronizer_io[T <:Data](proto: T, N: Int) extends Bundle {
    val clk_in = Input(Vec(math.pow(2,N).toInt,Clock()))
    val data_in = Input(Vec(math.pow(2,N).toInt,proto))
    val clk_out_mux_en = Input(Bool())
    val clk_out_mux_sel = Input(UInt(N.W))
    val clk_out = Output(Clock())
    val data_out = Output(Vec(math.pow(2,N).toInt,proto))
    override def cloneType = (new synchronizer_io(proto.cloneType,N)).asInstanceOf[this.type]
}

class synchronizer[T <:Data](proto: T, N: Int) extends Module {
    val io = IO(new synchronizer_io(proto=proto, N=N))
    val pow2N = math.pow(2,N).toInt

    //Input registers
    val stage1 = Wire(Vec(pow2N,proto))
    for (i <- 0 to pow2N-1) {
        val stage1_reg = withClock(io.clk_in(i)) { RegNext(io.data_in(i)) }
        stage1(i) := stage1_reg
    }

    //Synchronize half of the inputs to one clock and the other half to another clock
    val stage2 = Wire(Vec(pow2N,proto))
    for (i <- 0 to pow2N-1) {
        if (i < pow2N/2) {
            val stage2_reg = withClock(io.clk_in(pow2N-1)) { RegNext(stage1(i)) }
            stage2(i) := stage2_reg
        } else {
            val stage2_reg = withClock(io.clk_in(pow2N/2-1)) { RegNext(stage1(i)) }
            stage2(i) := stage2_reg
        }
    }

    //Synchronize all to the same clock
    val stage3 = withClock(io.clk_in(pow2N-1)) { RegNext(stage2) }

    //Convert input clocks to Bool for multiplexing
    val clk_in_bool = Wire(Vec(pow2N,Bool()))
    for (i <- 0 to pow2N-1) {
        clk_in_bool(i) := io.clk_in(i).asUInt.toBool
    }

    //Set outputs
    io.clk_out := Mux(io.clk_out_mux_en, clk_in_bool(io.clk_out_mux_sel), clk_in_bool(pow2N-1)).asClock()
    io.data_out := stage3
}

//This gives you verilog
object synchronizer extends App {
    chisel3.Driver.execute(args, () => new synchronizer(
        proto=DspComplex(UInt(4.W),UInt(4.W)), N=2)
    )
}

//This is a simple unit tester for demonstration purposes
class unit_tester(c: synchronizer[DspComplex[UInt]]) extends DspTester(c) {
}

//This is the test driver 
object unit_test extends App {
    iotesters.Driver.execute(args, () => new synchronizer(proto=DspComplex(UInt(4.W),UInt(4.W)), N=2)){
        c => new unit_tester(c)
    }
}
