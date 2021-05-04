package experimental.chimneylike.internal.derived

import experimental.chimneylike._
import experimental.chimneylike.dsl._
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal._
import scala.compiletime.ops.int._
import scala.compiletime._
import scala.deriving._

object CoproductDerive:
  import DeriveUtils._

  inline def derived[From, To, Path <: String](inline config: TypeDeriveConfig[_, _, Path])(using fm: Mirror.SumOf[From], tm: Mirror.SumOf[To]): Transformer[From, To] =
    DeriveUtils.transformerWith[From,  To] { from => findACase[From, To, fm.MirroredElemTypes, tm.MirroredElemTypes, fm.MirroredElemLabels, 0](config)(from) }
  end derived

  inline def derivedF[F[_], From, To, Path <: String](inline config: TypeDeriveConfig[_, _, Path])(using fm: Mirror.SumOf[From], tm: Mirror.SumOf[To], sup: TransformerFSupport[F]): TransformerF[F, From, To] =
    DeriveUtils.transformerWithF[F, From, To](from => findACaseF[F, From, To, fm.MirroredElemTypes, tm.MirroredElemTypes, fm.MirroredElemLabels, 0](config)(from))
  end derivedF

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
      case _: (EmptyTuple, _, _) =>
        throw new Exception("Should not be here, bug in implementation, report it")
      case _: (_, EmptyTuple, _) =>
        inline config match
          case _: TypeDeriveConfig[_, _, path] =>
            MacroUtils.reportErrorAtPathWithType[path, "Structure of transformed coproducts do not match", (From, To)]("Transforming coproducts")
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