package rocketchip

import Chisel._
import sys.process.stringSeqToProcess

abstract class RocketChipTestSuite(config: ChiselConfig, N: Int = 8) 
    extends org.scalatest.FlatSpec with org.scalatest.BeforeAndAfter { 
  import scala.actors.Actor._
  import java.io.{File, PrintStream}

  private val outDir = new File("test-outs")
  private val logDir = new File("test-logs")
  private val dumpDir = new File("test-dumps")
  private val baseArgs = Array("--W0W", "--targetDir", outDir.getPath.toString)
  private def compArgs(b: String)  = baseArgs ++ Array("--backend", b,
    "--genHarness", "--compile", "--noAssert", "--compileInitializationUnoptimized")
  private def debugArgs(b: String) = compArgs(b) ++ Array("--debug", "--vcd", "--vcdMem")
  private def testArgs(cmd: String) = baseArgs ++ Array(
    "--backend", "null", "--testCommand", cmd)
  protected val configName = config.getClass.getName stripPrefix "rocketchip."

  case class TestRun(c: Module, args: RocketChipTestArgs, sample: Option[String] = None)
  case class TopReplay(c: Top, sample: Seq[strober.Sample], log: String) 
  case object TestFin
  protected val testers = List.fill(N){ actor { loop { react {
    case TestRun(c, args, sample) => c match {
      case top: Top => 
        sender ! (new RocketChipTester(top, args)).finish
      case top: TopWrapper => 
        sender ! (new RocketChipSimTester(top, args, sample)).finish
      case top: NASTIShim =>
        sender ! (new RocketChipNastiShimTester(top, args, sample)).finish
      case _ => sender ! false
    }
    case TopReplay(c, sample, log) =>
      sender ! (new RocketChipReplay(c, sample, log=Some(log))).finish
    case TestFin => exit()
  } } } }

  def elaborate[T <: Module](c: => T, b: String, debug: Boolean, cmd: Option[String]) = {
    val args = cmd match {
      case Some(p) => testArgs(p) case None => if (debug) debugArgs(b) else compArgs(b) 
    }
    val params = Parameters.root(new Instance(config.topDefinitions,config.knobValues))
    chiselMain(args, () => Module(c)(params))
  }

  def elaborateCpp[T <: Module](c: => T, debug: Boolean=false) = elaborate(c, "c", debug, None)

  def elaborateVerilog[T <: Module](c: => T, debug: Boolean=false) = elaborate(c, "v", debug, None)

  def elaborateSimv[T <: Module](c: => T, dir: String) = {
    val simv = new java.io.File(s"${dir}/simv-${configName}")
    if (!simv.exists) assert(Seq("make", "-C", dir, s"CONFIG=${configName}").! == 0)
    dumpDir.listFiles foreach (_.delete)
    elaborate(c, "", false, Some(simv.getPath.toString))
  }


  def runSuites[T <: Module, S <: RocketTestSuite](c: T, suites: Iterable[S], maxcycles: Long = 100000000) {
    suites foreach { suite =>
      val dir = suite.dir stripPrefix "$(base_dir)/"
      val futures = suite.names.zipWithIndex map { case (t, i) =>
        val name = suite match {
          case s: AssemblyTestSuite  => s"${s.toolsPrefix}-${s.envName}-${t}"
          case s: BenchmarkTestSuite => s"${t}.riscv"
        }
        val loadmem = s"${dir}/${name}.hex"
        val sample = Some(s"${name}.sample")
        val log = new PrintStream(s"${logDir.getPath}/${c.getClass.getName}-${name}.log")
        val vcd = s"${dumpDir}/${c.getClass.getName}-${name}.vcd"
        val vpd = s"${dumpDir}/${c.getClass.getName}-${name}.vpd"
        val saif = s"${dumpDir}/${c.getClass.getName}-${name}.saif"
        val dump = Driver.backend match { 
          case _: VerilogBackend => Some(vpd)
          case _: CppBackend     => Some(vcd)
          case _                 => None
        }
        val cmd = Driver.testCommand map (x => 
          s"""vcd2saif -input ${vcd} -output ${saif} -pipe "${x} +vcdfile=${vcd} +vpdfile=${vpd}" """)
        val args = new RocketChipTestArgs(loadmem, maxcycles, cmd, Some(log), dump)
        if (!(new File(loadmem).exists)) assert(Seq("make", "-C", dir, s"${name}.hex").! == 0)
        name -> (testers(i % N) !! new TestRun(c, args, sample))
      }
      behavior of suite.makeTargetName
      futures foreach {case (name, f) =>
        f.inputChannel receive {
          case pass: Boolean => it should s"pass ${name}" in { assert(pass) }
        }
      }
    }
  }

  def replaySamples[S <: RocketTestSuite](top: Top, suites: Iterable[S], suffix: String) {
    suites foreach { suite =>
      val dir = suite.dir stripPrefix "$(base_dir)/"
      val futures = suite.names.zipWithIndex map { case (t, i) =>
        val name = suite match {
          case s: AssemblyTestSuite  => s"${s.toolsPrefix}-${s.envName}-${t}"
          case s: BenchmarkTestSuite => s"${t}.riscv"
        }
        val sample = strober.Sample.load(s"${outDir}/${name}.sample")
        val log = s"${logDir}/${name}-replay-${suffix}.log"
        name -> (testers(i % N) !! new TopReplay(top, sample, log))
      }
      behavior of s"[Replay] ${suite.makeTargetName} + ${suffix}"
      futures foreach {case (name, f) =>
        f.inputChannel receive {
          case pass: Boolean => it should s"replay sample from ${name}" in { assert(pass) }
        }
      }
    }
  }

  if (!logDir.exists) logDir.mkdir
  if (!dumpDir.exists) dumpDir.mkdir

  after {
    testers foreach (_ ! TestFin)
  }
}

// Rocket Tests
class RocketAsmCppTests extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateCpp(new Top), TestGeneration.asmSuites.values)
}

class RocketAsmVerilogTests extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateVerilog(new Top), TestGeneration.asmSuites.values)
}

class RocketAsmCppTestsDebug extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateCpp(new Top, true), TestGeneration.asmSuites.values)
}

class RocketAsmVerilogTestsDebug extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateVerilog(new Top, true), TestGeneration.asmSuites.values)
}

class RocketAsmRTLTests extends RocketChipTestSuite(new DefaultFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-rtl"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.asmSuites.values) 
}

class RocketAsmSYNTests extends RocketChipTestSuite(new DefaultFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-syn"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.asmSuites.values) 
}

class RocketAsmPARTests extends RocketChipTestSuite(new DefaultFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-par"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.asmSuites.values) 
}

class RocketBmarkCppTests extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateCpp(new Top), TestGeneration.bmarkSuites.values)
}

class RocketBmarkVerilogTests extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateVerilog(new Top), TestGeneration.bmarkSuites.values)
}

class RocketBmarkCppTestsDebug extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateCpp(new Top, true), TestGeneration.bmarkSuites.values)
}

class RocketBmarkVerilogTestsDebug extends RocketChipTestSuite(new DefaultFPGAConfig) {
  runSuites(elaborateVerilog(new Top, true), TestGeneration.bmarkSuites.values)
}

class RocketBmarkRTLTests extends RocketChipTestSuite(new DefaultFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-rtl"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.bmarkSuites.values) 
}

class RocketBmarkSYNTests extends RocketChipTestSuite(new DefaultFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-syn"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.bmarkSuites.values) 
}

class RocketBmarkPARTests extends RocketChipTestSuite(new DefaultFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-par"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.bmarkSuites.values) 
}

// BOOM Tests
class BOOMAsmCppTests extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateCpp(new Top), TestGeneration.asmSuites.values)
}

class BOOMAsmVerilogTests extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateVerilog(new Top), TestGeneration.asmSuites.values)
}

class BOOMAsmCppTestsDebug extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateCpp(new Top, true), TestGeneration.asmSuites.values)
}

class BOOMAsmVerilogTestsDebug extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateVerilog(new Top, true), TestGeneration.asmSuites.values)
}

class BOOMAsmRTLTests extends RocketChipTestSuite(new BOOMFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-rtl"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.asmSuites.values) 
}

class BOOMAsmSYNTests extends RocketChipTestSuite(new BOOMFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-syn"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.asmSuites.values) 
}

class BOOMAsmPARTests extends RocketChipTestSuite(new BOOMFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-par"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.asmSuites.values) 
}

class BOOMBmarkCppTests extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateCpp(new Top), TestGeneration.bmarkSuites.values)
}

class BOOMBmarkVerilogTests extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateVerilog(new Top), TestGeneration.bmarkSuites.values)
}

class BOOMBmarkCppTestsDebug extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateCpp(new Top, true), TestGeneration.bmarkSuites.values)
}

class BOOMBmarkVerilogTestsDebug extends RocketChipTestSuite(new BOOMFPGAConfig) {
  runSuites(elaborateVerilog(new Top, true), TestGeneration.bmarkSuites.values)
}

class BOOMBmarkRTLTests extends RocketChipTestSuite(new BOOMFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-rtl"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.bmarkSuites.values) 
}

class BOOMBmarkSYNTests extends RocketChipTestSuite(new BOOMFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-syn"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.bmarkSuites.values) 
}

class BOOMBmarkPARTests extends RocketChipTestSuite(new BOOMFPGAConfig, 4) {
  val dir = "strober-replay/vcs-sim-gl-par"
  runSuites(elaborateSimv(new Top, dir), TestGeneration.bmarkSuites.values) 
}

// Rocket SimWrapper Tests
class RocketSimCppTests extends RocketChipTestSuite(new RocketSimConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateCpp(new TopWrapper, true), suites)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}

class RocketSimVerilogTests extends RocketChipTestSuite(new RocketSimConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateVerilog(new TopWrapper, true), TestGeneration.bmarkSuites.values)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}

// Rocket NastiShim Tests
class RocketNastiShimCppTests extends RocketChipTestSuite(new RocketNastiConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateCpp(new NASTIShim, true), suites)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}

class RocketNastiShimVerilogTests extends RocketChipTestSuite(new RocketNastiConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateVerilog(new NASTIShim, true), suites)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}

// BOOM SimWrapper Tests
class BOOMSimCppTests extends RocketChipTestSuite(new BOOMSimConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateCpp(new TopWrapper, true), suites)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}

class BOOMSimVerilogTests extends RocketChipTestSuite(new BOOMSimConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateVerilog(new TopWrapper, true), TestGeneration.bmarkSuites.values)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}

// BOOM NastiShim Tests
class BOOMNastiShimCppTests extends RocketChipTestSuite(new BOOMNastiConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateCpp(new NASTIShim, true), suites)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}

class BOOMNastiShimVerilogTests extends RocketChipTestSuite(new BOOMNastiConfig) {
  val suites = TestGeneration.bmarkSuites.values
  runSuites(elaborateVerilog(new NASTIShim, true), suites)
  replaySamples(elaborateCpp(new Top), suites, "cpp")
  replaySamples(elaborateVerilog(new Top), suites, "verilog")
}
