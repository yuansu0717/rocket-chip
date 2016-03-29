package rocketchip

import Chisel._
import org.scalatest._

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
      case top: NastiShim => sender ! (try { 
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

  def runSuites[T <: Module, S <: RocketTestSuite](c: => T, maxcycles: Long = 100000000) {
    property("RocketChip should run the following test suites") { configMap =>
      val backend = configMap("BACKEND").toString
      val config = configMap("CONFIG").toString
      val suites = configMap("SUITES").toString match {
        case "asm"   => TestGeneration.asmSuites.values
        case "bmark" => TestGeneration.bmarkSuites.values
      }
      val debug = configMap("DEBUG").toString.toBoolean
      val cmdDir = configMap get "DIR" map (_.toString)
      val dut = elaborate(c, config, backend, debug, cmdDir)
      val dutName = dut.getClass.getSimpleName
      forAll(Table("RunSuites", suites.toSeq: _*)) { suite =>
        Given(suite.makeTargetName)
        val dir = suite.dir stripPrefix "$(base_dir)/"
        suite.names.zipWithIndex map { case (t, i) =>
          val name = suite match {
            case s: AssemblyTestSuite  => s"${s.toolsPrefix}-${s.envName}-${t}"
            case s: BenchmarkTestSuite => s"${t}.riscv"
          }
          val loadmem = s"${dir}/${name}.hex"
          val sample = Some(s"${name}.sample")
          val log = new PrintStream(s"${logDir.getPath}/${dutName}-${name}-${backend}.log")
          val vcd = s"${dumpDir}/${dutName}-${name}.vcd"
          val vpd = s"${dumpDir}/${dutName}-${name}.vpd"
          val saif = s"${dumpDir}/${dutName}-${name}.saif"
          val dump = backend match { case "c" => Some(vcd) case "v" => Some(vpd) case _ => None }
          val cmd = cmdDir map { dir => 
            val pipe = "${dir}/simv-${config} +vcdfile=${vcd} +vpdfile=${vpd}" 
            s"""vcd2saif -input ${vcd} -output ${saif} -pipe "${pipe}" """ }
          val testArgs = new RocketChipTestArgs(loadmem, maxcycles, cmd, Some(log), dump)
          if (!(new File(loadmem).exists)) assert(Seq("make", "-C", dir, s"${name}.hex").! == 0)
          name -> (testers(i % N) !! new TestRun(dut, testArgs, sample))
        } foreach {case (name, f) =>
          f.inputChannel receive {case pass: Boolean =>
            Then(s"should pass ${name}") 
            assert(pass)
          }
        }
      }
    }
  }

  def replaySamples[S <: RocketTestSuite](c: => Top) {
    property(s"RocketChip should replay the following test suites") { configMap =>
      val config = configMap("CONFIG").toString
      val suites = configMap("SUITES").toString match {
        case "asm"   => TestGeneration.asmSuites.values
        case "bmark" => TestGeneration.bmarkSuites.values
      }
      val dir = configMap get "DIR" map (_.toString)
      val dut = elaborate(c, config, "v", true, dir)
      forAll(Table("Replay", suites.toSeq:_*)) { suite =>
        Given(suite.makeTargetName)
        val dir = suite.dir stripPrefix "$(base_dir)/"
        suite.names.zipWithIndex map { case (t, i) =>
          val name = suite match {
            case s: AssemblyTestSuite  => s"${s.toolsPrefix}-${s.envName}-${t}"
            case s: BenchmarkTestSuite => s"${t}.riscv"
          }
          val sample = strober.Sample.load(s"${outDir}/${name}.sample")
          val log = s"${logDir}/replay-${name}.log"
          val dump = s"${dumpDir}/replay-${name}.vpd"
          name -> (testers(i % N) !! new TopReplay(dut, sample, dump, log))
        } foreach {case (name, f) =>
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

class RocketChipTests extends RocketChipTestSuite {
  runSuites(new Top)
}

class SimTests extends RocketChipTestSuite {
  runSuites(new TopWrapper)
}

class NastiShimTests extends RocketChipTestSuite {
  runSuites(new NastiShim)
}

class ReplayTests extends RocketChipTestSuite {
  replaySamples(new Top)
}
