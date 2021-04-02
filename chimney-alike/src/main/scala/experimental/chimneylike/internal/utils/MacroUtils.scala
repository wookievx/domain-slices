package experimental.chimneylike.internal.utils

import scala.quoted.{given, *}
import deriving.*, compiletime.*

object MacroUtils:

  inline def getDefaultParams[T]: Map[String, AnyRef] = ${ getDefaultParmasImpl[T] }
  //copied from : https://github.com/dotty-staging/upickle/blob/0213eea95b282b1e961b1d5ad68031365c9a8bb2/implicits/src-3/upickle/implicits/macros.scala
  def getDefaultParmasImpl[T: Type](using Quotes): Expr[Map[String, AnyRef]] =
    import quotes.reflect.*
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
    import quotes.reflect.*
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

  inline def extracNameFromSelector[To, T](inline code: To => T): String = ${extractNameFromSelectorImpl('code)}

  def extractNameFromSelectorImpl[To: Type, T: Type](code: Expr[To => T])(using Quotes): Expr[String] = 
    import quotes.reflect.*
    val extractors = new Extractors
    code.asTerm match
     case extractors.InlinedLambda(_, Select(_, name)) => Expr(name)
     case t => report.throwError(s"Illegal argument to extractor: ${code.show}, in tasty: $t")

  
  class Extractors(using val quotes: Quotes):
    //attempt to strip away consecutive inlines in AST and extract only final lambda
    import quotes.reflect.*

    object InlinedLambda:
      def unapply(arg: Term): Option[(List[ValDef], Term)] = 
        arg match
          case Inlined(_, _, Lambda(vals, term)) => Some((vals, term))
          case Inlined(_, _, nested) => InlinedLambda.unapply(nested)
          case t => None
    end InlinedLambda
  end Extractors

  inline def debug[T](inline any: T): T = ${ printImplMacro('any) }

  def printImplMacro[T: Type](any: Expr[T])(using qctx: Quotes): Expr[T] = {
    import qctx.reflect._
    println(Printer.TreeShortCode.show(any.asTerm))
    any
  }  

end MacroUtils