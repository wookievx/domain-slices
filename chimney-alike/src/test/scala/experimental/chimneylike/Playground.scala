package experimental.chimneylike

import internal.utils.MacroUtils
import utest._

object Playground extends TestSuite:
  val tests = Tests {
    "default parameters" - {
      "are extracted" - {
        val params = MacroUtils.getDefaultParams[MyDefaultingClass]
        params ==> Map("a" -> 42, "b" -> "lama", "c" -> 42L)
      }

      "are checked to exist" - {
        checkIfDefaultExistsCompileTime("a")
        MacroUtils.nameExistsIn[MyDefaultingClass]("a") ==> true
      }

      "are checked to not exist" - {
        // checkIfDefaultExistsCompileTime("z")
        MacroUtils.nameExistsIn[MyDefaultingClass]("z") ==> false
      }

    }

    "selectors" - {
      "provide name of the field" - {
        MacroUtils.extracNameFromSelector[MyDefaultingClass, Int](_.a) ==> "a"
      }
    }
  }

  import scala.compiletime.error
  import scala.compiletime.ops.string._
  import scala.compiletime.constValue

  inline def checkIfDefaultExistsCompileTime[N <: String](inline name: N): Unit = 
    inline if MacroUtils.nameExistsIn[MyDefaultingClass](name) then () else error("Failed to compile because name not found")

  case class MyDefaultingClass(a: Int = 42, b: String = "lama", c: Long = 42L)
end Playground
