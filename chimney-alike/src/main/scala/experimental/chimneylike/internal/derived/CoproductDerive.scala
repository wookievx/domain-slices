package experimental.chimneylike.internal.derived

import experimental.chimneylike.*
import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*
import scala.compiletime.ops.int.*
import scala.compiletime.*
import scala.deriving.*

object CoproductDerive:
  import DeriveUtils.*
  inline def derived[From, To, Path <: String](inline config: TypeDeriveConfig[_, _, Path])(using fm: Mirror.SumOf[From], tm: Mirror.SumOf[To]): Transformer[From, To] = 
    new Transformer[From, To]:
      def transform(from: From): To =
        findACase[From, To, fm.MirroredElemTypes, tm.MirroredElemTypes, fm.MirroredElemLabels, 0](config)(from)

  inline def derivedF[F[_], From, To, Path <: String](inline config: TypeDeriveConfig[_, _, Path])(using fm: Mirror.SumOf[From], tm: Mirror.SumOf[To], sup: TransformerFSupport[F]): TransformerF[F, From, To] = 
    new TransformerF[F, From, To]:
      def transform(from: From): F[To] =
        findACaseF[F, From, To, fm.MirroredElemTypes, tm.MirroredElemTypes, fm.MirroredElemLabels, 0](config)(from)

  inline def findACase[From, To, FromLeft <: Tuple, ToLeft <: Tuple, Labels <: Tuple, Position <: Int](inline config: TypeDeriveConfig[_, _, _])(from: From)(using fm: Mirror.SumOf[From]): To = 
    inline (erasedValue[FromLeft], erasedValue[ToLeft], erasedValue[Labels]) match
      case _: (from *: fromLeft, to *: toLeft, fromName *: labels) =>
        if fm.ordinal(from) == constValue[Position] then
          inline config match
            case config: TypeDeriveConfig[_, flags, path] =>
              inline SpecialDerive.deriveSpecialCases[from, to, flags, path Concat "." Concat fromName] match
                case Some(t: Transformer[from, to]) =>
                  t.asInstanceOf[Transformer[From, To]].transform(from)
                case _ =>  
                  TransformerDerive.deriveConfigured[from, to, path Concat "." Concat fromName](configOfAtPath[to, flags, path Concat "." Concat fromName](defaultDefinitionWithFlags))
                    .asInstanceOf[Transformer[From, To]]
                    .transform(from)
        else
          findACase[From, To, fromLeft, toLeft, labels, Position + 1](config)(from)
      case _: (EmptyTuple, EmptyTuple, _) => 
        throw new Exception("Should not be here, bug in implementation, report it")
      case _ =>
        inline config match
          case _: TypeDeriveConfig[_, _, path] =>
            MacroUtils.reportErrorAtPath(constValue[path], "Structure of transformed coproducts do not match")
  end findACase

  inline def findACaseF[F[_], From, To, FromLeft <: Tuple, ToLeft <: Tuple, Labels <: Tuple, Position <: Int](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    from: From
  )(using 
    fm: Mirror.SumOf[From],
    sup: TransformerFSupport[F]
  ): F[To] = 
    inline (erasedValue[FromLeft], erasedValue[ToLeft], erasedValue[Labels]) match
      case _: (from *: fromLeft, to *: toLeft, fromName *: labels) =>
        if fm.ordinal(from) == constValue[Position] then
          inline config match
            case config: TypeDeriveConfig[_, flags, path] =>
              inline SpecialDerive.deriveSpecialCases[from, to, flags, path Concat "." Concat fromName] match
                case Some(t: Transformer[from, to]) =>
                  sup.pure(t.asInstanceOf[Transformer[From, To]].transform(from))
                case Some(t: TransformerF[F, from, to]) =>
                  t.asInstanceOf[TransformerF[F, From, To]].transform(from)
                case _ =>  
                  TransformerDerive.deriveConfiguredF[F, from, to, path Concat "." Concat fromName](configOfAtPath[to, flags, path Concat "." Concat fromName](defaultDefinitionWithFlags))
                    .asInstanceOf[TransformerF[F, From, To]]
                    .transform(from)
        else
          findACaseF[F, From, To, fromLeft, toLeft, labels, Position + 1](config)(from)
      case _: (EmptyTuple, EmptyTuple, _) => 
        throw new Exception("Should not be here, bug in implementation, report it")
      case _ =>
        inline config match
          case _: TypeDeriveConfig[_, _, path] =>
            MacroUtils.reportErrorAtPath(constValue[path], "Structure of transformed coproducts do not match")
  end findACaseF
end CoproductDerive