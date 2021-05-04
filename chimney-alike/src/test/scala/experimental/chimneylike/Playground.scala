package experimental.chimneylike

import dsl._
import experimental.chimneylike.internal.utils
import internal.utils._
import internal._
import utest._

object Playground extends TestSuite:
  val tests = Tests {

    "extracting stuff" - {
      val resExpected = accessValue[String](MacroPlayground.Mock("42", 42), "theField")
      val resWrongType = accessValue[String](MacroPlayground.Mock("42", 42), "theSpecialField")
      val resNotFound = accessValue[Int](MacroPlayground.Mock("42", 42), "field")

      assert(
        resExpected == Right("42"),
        resWrongType == Left("illegal field type: scala.Int"),
        resNotFound == Left(s"not found: field")
      )
    }
  }

  inline def accessValue[Expected](mock: MacroPlayground.Mock, inline field: String): Either[String, Expected] =
    inline utils.ClassAcceessMacros.selectByName(mock, field) match
      case opt: Some[Expected] => Right(opt.get)
      case _: Some[t] => Left(s"illegal field type: ${MacroUtils.showTypeVal[t]}")
      case None => Left(s"not found: ${field}")

end Playground
