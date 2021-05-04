package experimental.chimneylike.internal.utils

import scala.quoted.{given, _}
import deriving._, compiletime._

object MacroPlayground:

  inline def selectValue(value: Mock): Unit = ${selectValueImpl('value)}

  inline def selectValueInline(inline value: Mock): Unit = ${selectValueImpl('value)}

  def selectValueImpl(value: Expr[Mock])(using q: Quotes): Expr[Unit] =
    import q.reflect._
    println('{$value.field}.asTerm)
    '{}
  end selectValueImpl

  case class Mock(field: String, specialField: Int):
    def theField: String = field
    def theSpecialField: Int = specialField
end MacroPlayground
