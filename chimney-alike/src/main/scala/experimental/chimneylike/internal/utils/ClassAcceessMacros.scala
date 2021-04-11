package experimental.chimneylike.internal.utils

import scala.quoted.{given, _}
import deriving._, compiletime._

object ClassAcceessMacros:

  transparent inline def selectByName[C](cValue: C, inline name: String) = ${selectByNameImpl('cValue, 'name) }

  private def selectByNameImpl[C: Type](cValue: Expr[C], name: Expr[String])(using q: Quotes): Expr[Any] =
    '{None}
  end selectByNameImpl

  private def findMethod[C: Type](name: String)(using q: Quotes): Option[q.reflect.TypeTree] =
    import q.reflect._
    val sym = TypeTree.of[C].symbol
    if sym.isClassDef then
      val methods: List[Symbol] = sym.declaredMethods
      val rawMethod = methods.collectFirst { case s if s.name == name => s }
      rawMethod.map(_.tree) match
        case Some(DefDef(_, _, typeTree, _)) => Some(typeTree)
        case _ => None
    else None
  end findMethod

end ClassAcceessMacros
