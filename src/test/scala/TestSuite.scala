package rocketchip

import Chisel._
import org.scalatest._

case class ElabArgs(b: String = "null", debug: Boolean = false, dir: Option[String] = None) 
abstract class RocketChipTestSuite(N: Int = 6) extends fixture.PropSpec with fixture.ConfigMapFixture 
    with prop.TableDrivenPropertyChecks with GivenWhenThen with BeforeAndAfter { 
  import scala.actors.Actor._
  import matchers.ShouldMatchers._
  import java.io.{File, PrintStream}
  import sys.process.stringSeqToProcess

  private val outDir = new File("test-outs")
  private val logDir = new File("test-logs")
  private val dumpDir = new File("test-dumps")
  private val baseArgs = Array("--W0W", "--targetDir", outDir.getPath.toString)
  private def compArgs(config: String, b: String)  = baseArgs ++ 
    Array("--configInstance", s"rocketchip.${config}", "--backend", b, 
          "--genHarness", "--compile", "--noAssert", "--compileInitializationUnoptimized")
  private def debugArgs(config: String, b: String) = compArgs(config, b) ++ 
    Array("--debug", "--vcd", "--vcdMem")
  private def testArgs(b: String, cmd: String) = baseArgs ++ Array("--backend", b, "--testCommand", cmd)

  private case class TestRun(c: Module, args: RocketChipTestArgs, sample: Option[String] = None)
  private case class TopReplay(c: Top, sample: Seq[strober.Sample], dump: String, log: String) 
  private case object TestFin
  private val testers = List.fill(N){ actor { loop { react {
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

  private def elaborate[T <: Module](c: => T, config: String, b: String, debug: Boolean, dir: Option[String]) = {
    val args = dir match {
      case None if debug => debugArgs(config, b)
      case None          => compArgs(config, b)
      case Some(p)       =>
        val simv = new File(s"${p}/simv-${config}")
        if (!simv.exists) assert(Seq("make", "-C", p, s"CONFIG=${config}").! == 0)
        dumpDir.listFiles foreach (_.delete)
        testArgs(b, simv.getPath.toString)
    }
    chiselMain.run(args, () => c)
  }

  def runSuites[T <: Module, S <: RocketTestSuite](
      c: => T, args: ElabArgs, suites: Iterable[S], maxcycles: Long = 100000000) {
    property("RocketChip should run the following test suites") { configMap =>
      val config = configMap("CONFIG").asInstanceOf[String]
      val dut = elaborate(c, config, args.b, args.debug, args.dir)
      forAll(Table("RunSuites", suites.toSeq: _*)) { suite =>
        val dir = suite.dir stripPrefix "$(base_dir)/"
        val futures = suite.names.zipWithIndex map { case (t, i) =>
          val name = suite match {
            case s: AssemblyTestSuite  => s"${s.toolsPrefix}-${s.envName}-${t}"
            case s: BenchmarkTestSuite => s"${t}.riscv"
          }
          val loadmem = s"${dir}/${name}.hex"
          val sample = Some(s"${name}.sample")
          val log = new PrintStream(s"${logDir.getPath}/${dut.getClass.getName}-${name}.log")
          val vcd = s"${dumpDir}/${dut.getClass.getName}-${name}.vcd"
          val vpd = s"${dumpDir}/${dut.getClass.getName}-${name}.vpd"
          val saif = s"${dumpDir}/${dut.getClass.getName}-${name}.saif"
          val dump = Driver.backend match { 
            case _: VerilogBackend => Some(vpd)
            case _: CppBackend     => Some(vcd)
            case _                 => None
          }
          val cmd = Driver.testCommand map (x => 
            s"""vcd2saif -input ${vcd} -output ${saif} -pipe "${x} +vcdfile=${vcd} +vpdfile=${vpd}" """)
          val args = new RocketChipTestArgs(loadmem, maxcycles, cmd, Some(log), dump)
          if (!(new File(loadmem).exists)) assert(Seq("make", "-C", dir, s"${name}.hex").! == 0)
          name -> (testers(i % N) !! new TestRun(dut, args, sample))
        }
        Given(suite.makeTargetName)
        futures.zipWithIndex foreach {case ((name, f), i) =>
          f.inputChannel receive {case pass: Boolean =>
            Then(s"should pass ${name}") 
            assert(pass)
          }
        }
      }
    }
  }

  def replaySamples[S <: RocketTestSuite](c: => Top, args: ElabArgs, suites: Iterable[S]) {
    property(s"[replay-${args.b}] RocketChip should replay the following test suites") { configMap =>
      val config = configMap("CONFIG").asInstanceOf[String]
      val dut = elaborate(c, config, args.b, args.debug, args.dir)
      forAll(Table("Replay", suites.toSeq:_*)) { suite =>
        val dir = suite.dir stripPrefix "$(base_dir)/"
        val futures = suite.names.zipWithIndex map { case (t, i) =>
          val name = suite match {
            case s: AssemblyTestSuite  => s"${s.toolsPrefix}-${s.envName}-${t}"
            case s: BenchmarkTestSuite => s"${t}.riscv"
          }
          val sample = strober.Sample.load(s"${outDir}/${name}.sample")
          val log = s"${logDir}/replay-${args.b}-${name}.log"
          val vcd = s"${dumpDir}/replay-${args.b}-${name}.vcd"
          val vpd = s"${dumpDir}/replay-${args.b}-${name}.vpd"
          val dump = Driver.backend match { 
            case _: VerilogBackend => vpd
            case _: CppBackend     => vcd
          }
          name -> (testers(i % N) !! new TopReplay(dut, sample, dump, log))
        }
        Given(suite.makeTargetName)
        futures foreach {case (name, f) =>
          f.inputChannel receive { case pass: Boolean => 
            Then(s"should replay sample from ${name}") 
            assert(pass)
          }
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
  def runSuites[T <: Module, S <: RocketTestSuite](
    c: => T, args: ElabArgs, suites: Iterable[S], maxcycles: Long = 100000000): Unit
  def replaySamples[S <: RocketTestSuite](c: => Top, args: ElabArgs, suites: Iterable[S]): Unit
}
trait AsmTests {
  val suites = TestGeneration.asmSuites.values
}
trait BmarkTests {
  val suites = TestGeneration.bmarkSuites.values
}
trait CppTests extends RunTests {
  runSuites(new Top, new ElabArgs("c"), suites)
}
trait VerilogTests extends RunTests {
  runSuites(new Top, new ElabArgs("v"), suites)
}
trait CppTestsDebug extends RunTests {
  runSuites(new Top, new ElabArgs("c", true), suites)
}
trait VerilogTestsDebug extends RunTests {
  runSuites(new Top, new ElabArgs("v", true), suites)
}
trait RTLTests extends RunTests {
  val dir = "strober-replay/vcs-sim-rtl"
  runSuites(new Top, new ElabArgs(dir=Some(dir)), suites)
}
trait SYNTests extends RunTests {
  val dir = "strober-replay/vcs-sim-gl-syn"
  runSuites(new Top, new ElabArgs(dir=Some(dir)), suites)
}
trait PARTests extends RunTests {
  val dir = "strober-replay/vcs-sim-gl-par"
  runSuites(new Top, new ElabArgs(dir=Some(dir)), suites)
}
class AsmCppTests extends RocketChipTestSuite() with AsmTests with CppTests
class AsmVerilogTests extends RocketChipTestSuite() with AsmTests with VerilogTests
class AsmCppTestsDebug extends RocketChipTestSuite() with AsmTests with CppTestsDebug
class AsmVerilogTestsDebug extends RocketChipTestSuite() with AsmTests with VerilogTestsDebug
class AsmRTLTests extends RocketChipTestSuite() with AsmTests with RTLTests
class AsmSYNTests extends RocketChipTestSuite() with AsmTests with SYNTests
class AsmPARTests extends RocketChipTestSuite() with AsmTests with PARTests
class BmarkCppTests extends RocketChipTestSuite() with BmarkTests with CppTests
class BmarkVerilogTests extends RocketChipTestSuite() with BmarkTests with VerilogTests
class BmarkCppTestsDebug extends RocketChipTestSuite() with BmarkTests with CppTestsDebug
class BmarkVerilogTestsDebug extends RocketChipTestSuite() with BmarkTests with VerilogTestsDebug
class BmarkRTLTests extends RocketChipTestSuite() with BmarkTests with RTLTests
class BmarkSYNTests extends RocketChipTestSuite() with BmarkTests with SYNTests
class BmarkPARTests extends RocketChipTestSuite() with BmarkTests with PARTests

trait SimCppTests extends RunTests {
  runSuites(new TopWrapper, new ElabArgs("c", true), suites)
}
trait SimVerilogTests extends RunTests {
  runSuites(new TopWrapper, new ElabArgs("v", true), suites)
}
trait NastiShimCppTests extends RunTests {
  runSuites(new NASTIShim, new ElabArgs("c", true), suites)
}
trait NastiShimVerilogTests extends RunTests {
  runSuites(new NASTIShim, new ElabArgs("v", true), suites)
}
trait ReplayTests extends RunTests {
  // replaySamples(new Top, new ElabArgs("c", true), suites)
  replaySamples(new Top, new ElabArgs("v", true), suites)
}
class SimAsmCppTests extends RocketChipTestSuite() with AsmTests with SimCppTests with ReplayTests
class SimAsmVerilogTests extends RocketChipTestSuite() with AsmTests with SimVerilogTests with ReplayTests
class SimBmarkCppTests extends RocketChipTestSuite() with BmarkTests with SimCppTests with ReplayTests
class SimBmarkVerilogTests extends RocketChipTestSuite() with BmarkTests with SimVerilogTests with ReplayTests
class NastiShimAsmCppTests extends RocketChipTestSuite() with AsmTests with NastiShimCppTests with ReplayTests
class NastiShimAsmVerilogTests extends RocketChipTestSuite() with AsmTests with NastiShimVerilogTests with ReplayTests
class NastiShimBmarkCppTests extends RocketChipTestSuite() with BmarkTests with NastiShimCppTests with ReplayTests
class NastiShimBmarkVerilogTests extends RocketChipTestSuite() with BmarkTests with NastiShimVerilogTests with ReplayTests

class AllRocketChipTests extends Suites(
  new AsmCppTests, new AsmVerilogTests, new BmarkCppTests, new BmarkVerilogTests)
