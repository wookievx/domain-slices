package util

import scala.quoted.*

object DebugUtils with

  inline def debug[T](inline any: T): T = ${ printImplMacro('any) }

  def printImplMacro[T: Type](any: Expr[T])(using qctx: Quotes): Expr[T] = {
    import qctx.reflect._
    println(Printer.TreeShortCode.show(any.asTerm))
    any
  }

end DebugUtils