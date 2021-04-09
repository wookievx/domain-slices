package experimental.chimneylike

import dsl._
import internal.utils.MacroUtils
import internal._
import utest._

object Playground extends TestSuite:
  val tests = Tests {
    "default parameters" - {
      "are extracted" - {
        val params = MacroUtils.getDefaultParams[MyDefaultingClass]
        params ==> Map("a" -> 42, "b" -> "lama", "c" -> 42L)
      }

      "are checked to not exist" - {
        // checkIfDefaultExistsCompileTime("z")
        MacroUtils.defaultValueExistsIn[MyDefaultingClass]("z") ==> false
      }

    }

    "selectors" - {
      "provide name of the field" - {
        MacroUtils.extracNameFromSelector[MyDefaultingClass, Int](_.a) ==> "a"
      }
    }

    "TransformerDefinition" - {
      "adds withFieldConst to config" - {
        val instance: TransformerDefinition[MySourceClass, MyDefaultingClass, (TransformerCfg.FieldConst["c"], TransformerCfg.FieldConst["a"]), EmptyTuple] = 
          defaultDefinition[MySourceClass, MyDefaultingClass]
              .withFieldConst(_.c, 9000L)
              .withFieldConst(_.a, 2625)
              .withFieldConst(_.c, 420L)

        instance.overrides.get("c") ==> Some(420L)
        instance.overrides.get("a") ==> Some(2625)
      }

      "adds withFieldFConst to config" - {
        type Configuration = (TransformerCfg.FieldConstF["c"], TransformerCfg.FieldConstF["a"], TransformerCfg.WrapperType[Option])

        val instance: TransformerFDefinition[Option, MySourceClass, MyDefaultingClass, Configuration, EmptyTuple] =
          defaultDefinition[MySourceClass, MyDefaultingClass]
              .withFieldConstF(_.c, Option(9000L))
              .withFieldConstF(_.a, Some(2625))
              .withFieldConstF(_.c, Some(420L))

        instance.overrides.get("c") ==> Some(Some(420L))
        instance.overrides.get("a") ==> Some(Some(2625))
      }

      "combine diferent types of configs and flags" - {
        import TransformerCfg._
        type ExpectedConfig = 
          (FieldComputedF["d"], FieldComputed["c"], FieldConst["a"], FieldConstF["b"], WrapperType[Option])

        type ExpectedFlags =
          (TransformerFlag.MethodAccessors, TransformerFlag.DefaultValues)
        
        val instance: TransformerFDefinition[Option, MySourceClass, MyDefaultingClass, ExpectedConfig, ExpectedFlags] =
          defaultDefinition[MySourceClass, MyDefaultingClass]
            .withFieldConstF(_.b, Option("pig"))
            .withFieldConst(_.c, 420L)
            .withFieldConst(_.a, 420)
            .withFieldComputed(_.c, s => (2 * s.a).toLong)
            .withFieldComputedF(_.d, s => s.toString.toDoubleOption)
            .enableDefaultValues
            .enableMethodAccessors

        instance.overrides("c")
          .asInstanceOf[MySourceClass => Long](MySourceClass(100, "test")) ==> 200L
        instance.overrides("b") ==> Some("pig")
        instance.overrides.get("a") ==> Some(420)
        instance.overrides("d")
          .asInstanceOf[MySourceClass => Option[Double]](MySourceClass(100, "test")) ==> None

      }

      "adds/removes any flag and compiles" - {
        type ExampleFlags = (TransformerFlag.MethodAccessors, TransformerFlag.DefaultValues, TransformerFlag.BeanGetters)

        val instance: TransformerDefinition[MySourceClass, MyDefaultingClass, EmptyTuple, ExampleFlags] =
          defaultDefinition[MySourceClass, MyDefaultingClass]
            .enableBeanGetters
            .enableBeanSetters
            .enableDefaultValues
            .enableMethodAccessors
            .disableBeanSetters

        true ==> true

      }
    }

  }

  case class MySourceClass(a: Int, b: String)
  case class MyDefaultingClass(a: Int = 42, b: String = "lama", c: Long = 42L, d: Double)
end Playground
