package experimental.chimneylike.internal.utils

import scala.quoted.{given, *}
import deriving._, compiletime._

object MacroUtils:

  inline def getDefaultParams[T]: Map[String, AnyRef] = ${ getDefaultParmasImpl[T] }
  //copied from : https://github.com/dotty-staging/upickle/blob/0213eea95b282b1e961b1d5ad68031365c9a8bb2/implicits/src-3/upickle/implicits/macros.scala
  def getDefaultParmasImpl[T: Type](using Quotes): Expr[Map[String, AnyRef]] =
    import quotes.reflect._
    val sym = TypeTree.of[T].symbol

    if (sym.isClassDef) {
      val comp = if (sym.isClassDef) sym.companionClass else sym
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name
      val namesExpr: Expr[List[String]] =
        Expr.ofList(names.map(Expr(_)))

      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Ref] =
        for case deff @ DefDef(name, _, _, _) <- body if name.startsWith("$lessinit$greater$default")
        yield Ref(deff.symbol)
      val identsExpr: Expr[List[Any]] =
        Expr.ofList(idents.map(_.asExpr))

      '{ $namesExpr.zip($identsExpr.map(_.asInstanceOf[AnyRef])).toMap }
    } else {
      '{ Map.empty }
    }
  end getDefaultParmasImpl

  inline def nameExistsIn[T](inline name: String): Boolean = ${ nameExistsInImpl[T]('name) }

  def nameExistsInImpl[T: Type](name: Expr[String])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    val sym = TypeTree.of[T].symbol

    if (sym.isClassDef) {
      val comp = if (sym.isClassDef) sym.companionClass else sym
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name

      name.value match
        case Some(name) => Expr(names.contains(name))
        case None => report.throwError("Failed to check if name exist, probably a bug in library")
    } else {
      Expr(false)
    }
  end nameExistsInImpl

end MacroUtils