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

  inline def defaultValueExistsIn[T](inline name: Any): Boolean = ${ nameExistsInImpl[T]('name) }

  def nameExistsInImpl[T: Type](name: Expr[Any])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    val sym = TypeTree.of[T].symbol

    if (sym.isClassDef) {
      val comp = if (sym.isClassDef) sym.companionClass else sym
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name

      name match 
        case '{$n: String} =>
          n.value match
            case Some(name) =>
              Expr(names.contains(name))
            case _ => report.throwError("Failed to check if name exist, probably a bug in library") 
        case _ =>
          report.throwError("Failed to check if name exist, probably a bug in library")
    } else {
      Expr(false)
    }
  end nameExistsInImpl

  transparent inline def extracNameFromSelector[To, T](inline code: To => T) = ${extractNameFromSelectorImpl('code)}

  def extractNameFromSelectorImpl[To: Type, T: Type](code: Expr[To => T])(using Quotes): Expr[String] = 
    import quotes.reflect._
    val extractors = new Extractors
    code.asTerm match
     case extractors.InlinedLambda(_, Select(_, name)) => Expr(name)
     case t => report.throwError(s"Illegal argument to extractor: ${code.show}, in tasty: $t")

  
  class Extractors(using val quotes: Quotes):
    //attempt to strip away consecutive inlines in AST and extract only final lambda
    import quotes.reflect._

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

  transparent inline def showTypeVal[T] = ${showTypeImpl[T]}

  private def showTypeImpl[T: Type](using quotes: Quotes): Expr[String] =
    Expr(Type.show[T])

  inline def showType[T]: Unit = ${ printType[T] }
  private def printType[T: Type](using quotes: Quotes): Expr[Unit] =
    println(s"Got type: ${Type.show[T]}")
    '{}
  end printType

  transparent inline def summonProductOf[T] = ${attemptSummonMirror[T]}

  private def attemptSummonMirror[T: Type](using q: Quotes): Expr[Any] = {
    import q.reflect.report
    Expr.summon[Mirror.ProductOf[T]] match
      case Some(product) => product
      case None => report.throwError(s"Failed to summon product of t: ${Type.show[T]}")
  }

  transparent inline def attemptSummonInstance[T] = ${attemptSummonInstanceImpl[T]}

  private def attemptSummonInstanceImpl[T: Type](using q: Quotes): Expr[Option[T]] = {
    import q.reflect.report
    Expr.summon[T] match
      case Some(instance) => '{ Some($instance) }
      case None => '{ None }
  }

  inline def reportErrorAtPathWithType[P <: String, Error <: String, T](inline constantPart: String) = ${ reportErrorAtPathWithTypeImpl[P, Error, T]('constantPart) }

  inline def reportErrorAtPath[P <: String](inline path: P, inline constantPart: String) = ${ reportErrorAtPathImpl('path, 'constantPart) }

  private def reportErrorAtPathWithTypeImpl[P <: String: Type, Error <: String: Type, T: Type](constantPart: Expr[String])(using q: Quotes): Expr[Nothing] =
    import q.reflect.report
    (Type.valueOfConstant[P], constantPart.value, Type.valueOfConstant[Error]) match
      case (Some(path), Some(constantPart), Some(error)) =>
        report.throwError(s"$constantPart at $path, type in question: ${Type.show[T]}, error: ${error}")
      case _ =>
        report.throwError("Unable to produce nice error, bug in library")

  end reportErrorAtPathWithTypeImpl

  private def reportErrorAtPathImpl[P <: String](path: Expr[P], constantPart: Expr[String])(using q: Quotes): Expr[Nothing] = {
    import q.reflect.report
    (path.value, constantPart.value) match
      case (Some(path), Some(v)) => 
        report.throwError(s"$v at $path")
      case _ =>
        report.throwError("Unable to produce nice error, bug in library")
  }

  inline def reportPointOfDerivation[P <: String](inline path: P) = ${reportPointOfDerivationImpl('path)}

  private def reportPointOfDerivationImpl[P <: String](path: Expr[P])(using Quotes): Expr[Unit] = {
    path.value match
      case Some(path) =>
        println(s"Automatic derivation at $path")
        '{}
      case None =>
        '{}
  }

  inline def printAtCompileTime[P <: String] = ${printAtCompileTimeImpl[P]}

  private def printAtCompileTimeImpl[P <: String: Type](using Quotes): Expr[Unit] =
    Type.valueOfConstant[P] match
      case Some(p) =>
        println(p)
        '{}
      case None =>
        '{}

end MacroUtils