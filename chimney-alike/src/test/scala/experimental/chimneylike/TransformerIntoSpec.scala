package experimental.chimneylike

import dsl.*
import internal.utils.MacroUtils
import internal.*
import utest.*

object TransformerIntoSpec extends TestSuite:
  val tests = Tests {
      "TransformerInto" - {
        import TransformerCfg.*
        import TransformerFlag.*
        "builds correctly typed definition" - {
          val source = Source(a = 100, b = "100", d = 42L, e = 6.6)

          type ExpectedConfig = (FieldComputedF["s"], FieldComputed["e"], FieldConstF["d"], FieldConst["b"], WrapperType[Option])
          type ExpectedFlags = (MethodAccessors, OptionDefaultsToNone)

          val transformation: TransformerFInto[Option, Source, Target, ExpectedConfig, ExpectedFlags] =
            source
              .into[Target]
              .withFieldConstF(_.s, Option.empty[Source])
              .withFieldConst(_.b, "200")
              .withFieldConstF(_.d, Some(420L))
              .withFieldComputed(_.e, source => (source.a + source.d + source.e) / 3)
              .withFieldComputedF(_.s, source => Some(source.copy(a = 200)))
              .enableOptionDefaultsToNone
              .enableMethodAccessors
          
          transformation.definition.overrides.get("b") ==> Some("200")
          transformation.definition.overrides("d") ==> Some(420L)
          transformation.definition.overrides("e")
            .asInstanceOf[Source => Double](source) ==> (source.a + source.d + source.e) / 3
          transformation.definition.overrides("s")
            .asInstanceOf[Source => Option[Source]](source) ==> Some(source.copy(a = 200))
          
        }
      }
  }

  case class Source(a: Int, b: String, d: Long, e: Double)
  case class Target(a: Int, b: String, d: Long, e: Double, s: Source)
end TransformerIntoSpec