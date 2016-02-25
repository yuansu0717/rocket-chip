package rocketchip

import Chisel._
import sys.process.stringSeqToProcess

abstract class RocketChipTestSuite(config: ChiselConfig, N: Int = 6) 
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
  private val configName = config.getClass.getName stripPrefix "rocketchip."

  case class TestRun(c: Module, args: RocketChipTestArgs, sample: Option[String] = None)
  case class TopReplay(c: Top, sample: Seq[strober.Sample], dump: String, log: String) 
  case object TestFin
  protected val testers = List.fill(N){ actor { loop { react {
    case TestRun(c, args, sample) => c match {
      case top: Top => sender ! (try { 
        (new RocketChipTester(top, args)).finish
      } catch { 
        case _: Throwable => false 
      })
      case top: TopWrapper => sender ! (try { 
        (new RocketChipSimTester(top, args, sample)).finish
      } catch { 
        case _: Throwable => false 
      })
      case top: NASTIShim => sender ! (try { 
        (new RocketChipNastiShimTester(top, args, sample)).finish
      } catch { 
        case _: Throwable => false 
      })
      case _ => sender ! false
    }
    case TopReplay(c, sample, dump, log) => sender ! (try { 
      (new RocketChipReplay(c, sample, dump=Some(dump), log=Some(log))).finish
    } catch { 
      case _: Throwable => false 
    })
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
        val log = s"${logDir}/replay-${suffix}-${name}.log"
        val vcd = s"${dumpDir}/replay-${suffix}-${name}.vcd"
        val vpd = s"${dumpDir}/replay-${suffix}-${name}.vpd"
        val dump = Driver.backend match { 
          case _: VerilogBackend => vpd
          case _: CppBackend     => vcd
        }
        name -> (testers(i % N) !! new TopReplay(top, sample, dump, log))
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

// Test Templates
trait RunTests {
  def suites: Iterable[RocketTestSuite]
  def elaborateCpp[T <: Module](c: => T, debug: Boolean=false): T
  def elaborateVerilog[T <: Module](c: => T, debug: Boolean=false): T
  def elaborateSimv[T <: Module](c: => T, dir: String): T
  def runSuites[T <: Module, S <: RocketTestSuite](c: T, suites: Iterable[S], maxcycles: Long = 100000000): Unit
  def replaySamples[S <: RocketTestSuite](top: Top, suites: Iterable[S], suffix: String): Unit
}
trait AsmTests {
  val suites = TestGeneration.asmSuites.values
}
trait BmarkTests {
  val suites = TestGeneration.bmarkSuites.values
}
trait CppTests extends RunTests {
  runSuites(elaborateCpp(new Top), suites)
}
trait VerilogTests extends RunTests {
  runSuites(elaborateVerilog(new Top), suites)
}
trait CppTestsDebug extends RunTests {
  runSuites(elaborateCpp(new Top, true), suites)
}
trait VerilogTestsDebug extends RunTests {
  runSuites(elaborateVerilog(new Top, true), suites)
}
trait RTLTests extends RunTests {
  val dir = "strober-replay/vcs-sim-rtl"
  runSuites(elaborateSimv(new Top, dir), suites)
}
trait SYNTests extends RunTests {
  val dir = "strober-replay/vcs-sim-gl-syn"
  runSuites(elaborateSimv(new Top, dir), suites)
}
trait PARTests extends RunTests {
  val dir = "strober-replay/vcs-sim-gl-par"
  runSuites(elaborateSimv(new Top, dir), suites)
}
trait AsmCppTests extends AsmTests with CppTests
trait AsmVerilogTests extends AsmTests with VerilogTests
trait AsmCppTestsDebug extends AsmTests with CppTestsDebug
trait AsmVerilogTestsDebug extends AsmTests with VerilogTestsDebug
trait AsmRTLTests extends AsmTests with RTLTests
trait AsmSYNTests extends AsmTests with SYNTests
trait AsmPARTests extends AsmTests with PARTests
trait BmarkCppTests extends BmarkTests with CppTests
trait BmarkVerilogTests extends BmarkTests with VerilogTests
trait BmarkCppTestsDebug extends BmarkTests with CppTestsDebug
trait BmarkVerilogTestsDebug extends BmarkTests with VerilogTestsDebug
trait BmarkRTLTests extends BmarkTests with RTLTests
trait BmarkSYNTests extends BmarkTests with SYNTests
trait BmarkPARTests extends BmarkTests with PARTests

trait SimCppTests extends RunTests {
  runSuites(elaborateCpp(new TopWrapper, true), suites)
}
trait SimVerilogTests extends RunTests {
  runSuites(elaborateVerilog(new TopWrapper, true), suites)
}
trait NastiShimCppTests extends RunTests {
  runSuites(elaborateCpp(new NASTIShim, true), suites)
}
trait NastiShimVerilogTests extends RunTests {
  runSuites(elaborateVerilog(new NASTIShim, true), suites)
}
trait ReplayTests extends RunTests {
  // replaySamples(elaborateCpp(new Top, true), suites, "cpp")
  replaySamples(elaborateVerilog(new Top, true), suites, "verilog")
}
trait SimAsmCppTests extends AsmTests with SimCppTests with ReplayTests
trait SimAsmVerilogTests extends AsmTests with SimVerilogTests with ReplayTests
trait SimBmarkCppTests extends BmarkTests with SimCppTests with ReplayTests
trait SimBmarkVerilogTests extends BmarkTests with SimVerilogTests with ReplayTests
trait NastiShimAsmCppTests extends AsmTests with NastiShimCppTests with ReplayTests
trait NastiShimAsmVerilogTests extends AsmTests with NastiShimVerilogTests with ReplayTests
trait NastiShimBmarkCppTests extends BmarkTests with NastiShimCppTests with ReplayTests
trait NastiShimBmarkVerilogTests extends BmarkTests with NastiShimVerilogTests with ReplayTests

// Rocket Tests
// Change config for different params
abstract class RocketTestsBase extends RocketChipTestSuite(new DefaultFPGAConfig)
class RocketAsmCppTests extends RocketTestsBase with AsmCppTests
class RocketAsmVerilogTests extends RocketTestsBase with AsmVerilogTests
class RocketAsmCppTestsDebug extends RocketTestsBase with AsmCppTestsDebug
class RocketAsmVerilogTestsDebug extends RocketTestsBase with AsmVerilogTestsDebug
class RocketAsmRTLTests extends RocketTestsBase with AsmRTLTests
class RocketAsmSYNTests extends RocketTestsBase with AsmSYNTests
class RocketAsmPARTests extends RocketTestsBase with AsmPARTests
class RocketBmarkCppTests extends RocketTestsBase with BmarkCppTests
class RocketBmarkVerilogTests extends RocketTestsBase with BmarkVerilogTests
class RocketBmarkCppTestsDebug extends RocketTestsBase with BmarkCppTestsDebug
class RocketBmarkVerilogTestsDebug extends RocketTestsBase with BmarkVerilogTestsDebug
class RocketBmarkRTLTests extends RocketTestsBase with BmarkRTLTests
class RocketBmarkSYNTests extends RocketTestsBase with BmarkSYNTests
class RocketBmarkPARTests extends RocketTestsBase with BmarkPARTests

// BOOM Tests
abstract class BOOMTestsSuite extends RocketChipTestSuite(new BOOMFPGAConfig)
class BOOMAsmCppTests extends BOOMTestsSuite with AsmCppTests
class BOOMAsmVerilogTests extends BOOMTestsSuite with AsmVerilogTests
class BOOMAsmCppTestsDebug extends BOOMTestsSuite with AsmCppTestsDebug
class BOOMAsmVerilogTestsDebug extends BOOMTestsSuite with AsmVerilogTestsDebug
class BOOMAsmRTLTests extends BOOMTestsSuite with AsmRTLTests
class BOOMAsmSYNTests extends BOOMTestsSuite with AsmSYNTests
class BOOMAsmPARTests extends BOOMTestsSuite with AsmPARTests
class BOOMBmarkCppTests extends BOOMTestsSuite with BmarkCppTests
class BOOMBmarkVerilogTests extends BOOMTestsSuite with BmarkVerilogTests
class BOOMBmarkCppTestsDebug extends BOOMTestsSuite with BmarkCppTestsDebug
class BOOMBmarkVerilogTestsDebug extends BOOMTestsSuite with BmarkVerilogTestsDebug
class BOOMBmarkRTLTests extends BOOMTestsSuite with BmarkRTLTests
class BOOMBmarkSYNTests extends BOOMTestsSuite with BmarkSYNTests
class BOOMBmarkPARTests extends BOOMTestsSuite with BmarkPARTests

// Rocket SimWrapper Tests
abstract class RocketSimTestsBase extends RocketChipTestSuite(new RocketSimConfig)
class RocketSimAsmCppTests extends RocketSimTestsBase with SimAsmCppTests
class RocketSimAsmVerilogTests extends RocketSimTestsBase with SimAsmVerilogTests
class RocketSimBmarkCppTests extends RocketSimTestsBase with SimBmarkCppTests
class RocketSimBmarkVerilogTests extends RocketSimTestsBase with SimBmarkVerilogTests

// Rocket NastiShim Tests
abstract class RocketNastiShimTestsBase extends RocketChipTestSuite(new RocketNastiConfig)
class RocketNastiShimAsmCppTests extends RocketNastiShimTestsBase with NastiShimAsmCppTests
class RocketNastiShimAsmVerilogTests extends RocketNastiShimTestsBase with NastiShimAsmVerilogTests
class RocketNastiShimBmarkCppTests extends RocketNastiShimTestsBase with NastiShimBmarkCppTests
class RocketNastiShimBmarkVerilogTests extends RocketNastiShimTestsBase with NastiShimBmarkVerilogTests

// BOOM SimWrapper Tests
abstract class BOOMSimTestsBase extends RocketChipTestSuite(new BOOMSimConfig)
class BOOMSimAsmCppTests extends BOOMSimTestsBase with SimAsmCppTests
class BOOMSimAsmVerilogTests extends BOOMSimTestsBase with SimAsmVerilogTests
class BOOMSimBmarkCppTests extends BOOMSimTestsBase with SimBmarkCppTests
class BOOMSimBmarkVerilogTests extends BOOMSimTestsBase with SimBmarkVerilogTests

// BOOM NastiShim Tests
class BOOMNastiShimTestsBase extends RocketChipTestSuite(new BOOMNastiConfig) 
class BOOMNastiShimAsmCppTests extends BOOMNastiShimTestsBase with NastiShimAsmCppTests
class BOOMNastiShimAsmVerilogTests extends BOOMNastiShimTestsBase with NastiShimAsmVerilogTests
class BOOMNastiShimBmarkCppTests extends BOOMNastiShimTestsBase with NastiShimBmarkCppTests
class BOOMNastiShimBmarkVerilogTests extends BOOMNastiShimTestsBase with NastiShimBmarkVerilogTests
