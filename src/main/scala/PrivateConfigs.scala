package rocketchip

import Chisel._
import uncore._
import rocket._
import boom._
import DefaultTestSuites._
import strober._
import junctions._
import cde.{Parameters, Config, Dump, Knob}

class WithAllBooms extends Config(
  (pname,site,here) => pname match {
    case BuildTiles => {
      TestGeneration.addSuites(rv64i.map(_("p")))
      TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64u.map(_(env))))
      TestGeneration.addSuites(if(site(NTiles) > 1) List(mtBmarks, bmarks) else List(bmarks))
      List.fill(site(NTiles)){ (r: Bool, p: Parameters) =>
         Module(new BOOMTile(resetSignal = r)(p.alterPartial({case TLId => "L1toL2"})))
    }
  }}
)

// TODO: update to the latest parameterization system
//class WithBoomAndRocketAlternating extends Config(
//  (pname,site,here) => pname match {
//    case BuildTiles => {
//      TestGeneration.addSuites(rv64i.map(_("p")))
//      TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64u.map(_(env))))
//      TestGeneration.addSuites(if(site(NTiles) > 1) List(mtBmarks, bmarks) else List(bmarks))
//      (0 until site(NTiles)).map { i =>
//        (r:Bool) => Module({
//          if(i % 2 != 0) new RocketTile(resetSignal = r)
//          else           new BOOMTile(resetSignal = r)
//        }, {case TLId => "L1ToL2"})}}})

class SmallBOOMConfig  extends Config(new WithSmallBOOMs  ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultConfig)
class MediumBOOMConfig extends Config(new WithMediumBOOMs ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)
class MegaBOOMConfig   extends Config(new WithMegaBOOMs   ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)

class BOOMConfig extends Config(new WithMediumBOOMs ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultL2Config)
class BOOMVLSIConfig extends Config(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultVLSIConfig ++ new WithNoBoomCounters)
class BOOMFPGAConfig extends Config(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultFPGAConfig)
class BOOMCPPConfig extends  Config(new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultCPPConfig)

//class HeterogenousBoomConfig extends Config(new WithBoomAndRocketAlternating ++ new BOOMFPGAConfig)

class BOOMStroberConfig extends Config(new WithNoBoomCounters ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultFPGAConfig)
class BOOMSmallStroberConfig extends Config(new WithNoBoomCounters ++ new WithSmallBOOMs ++ new WithAllBooms ++ new DefaultBOOMConfig ++ new DefaultFPGAConfig)

// Strober
class SimConfig extends Config(
  (pname,site,here) => pname match {
    case SampleNum    => 30
    case TraceMaxLen  => 1024
    case DaisyWidth   => 32
    case ChannelLen   => 16
    case ChannelWidth => 32
  }
)

class NastiConfig extends Config(
  topDefinitions = (pname,site,here) => pname match {
    case NastiKey => try {
      site(NastiType) match {
        case NastiMaster => NastiParameters(32, 32, 12)
        case NastiSlave  => NastiParameters(64, 32, 6)
      }
    } catch {
      case e: cde.ParameterUndefinedException =>
        throw new scala.MatchError(pname)
    }
    case MemMaxCycles => 256
    case UseDRAMCounters => true
  }
)

class RocketSimConfig extends Config(new DefaultFPGAConfig ++ new SimConfig)
class RocketNastiConfig extends Config(new NastiConfig ++ new RocketSimConfig)
class RocketL2SimConfig extends Config(new DefaultL2FPGAConfig ++ new SimConfig)
class RocketL2NastiConfig extends Config(new NastiConfig ++ new RocketL2SimConfig)

class BOOMSimConfig extends Config(new BOOMStroberConfig ++ new SimConfig)
class BOOMNastiConfig extends Config(new NastiConfig ++ new BOOMSimConfig)
class BOOMSmallSimConfig extends Config(new BOOMSmallStroberConfig ++ new SimConfig)
class BOOMSmallNastiConfig extends Config(new NastiConfig ++ new BOOMSmallSimConfig)
