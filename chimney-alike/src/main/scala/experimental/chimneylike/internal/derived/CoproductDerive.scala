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
        findACase[From, To, fm.MirroredElemTypes, tm.MirroredElemTypes, 0](config)(from)

  inline def findACase[From, To, FromLeft <: Tuple, ToLeft <: Tuple, Position <: Int](inline config: TypeDeriveConfig[_, _, _])(from: From)(using fm: Mirror.SumOf[From]): To = 
    inline (erasedValue[FromLeft], erasedValue[ToLeft]) match
      case _: (from *: fromLeft, to *: toLeft) =>
        if fm.ordinal(from) == constValue[Position] then
          summonFrom {
            case t: Transformer[from, to] =>
              t.asInstanceOf[Transformer[From, To]].transform(from)
            case _ =>
              inline config match
                case config: TypeDeriveConfig[_, flags, path] =>
                  inline SpecialDerive.deriveSpecialCases[from, to, flags, path Concat ".OneOf"] match
                    case Some(t: Transformer[from, to]) =>
                      t.asInstanceOf[Transformer[From, To]].transform(from)
                    case _ =>  
                      TransformerDerive.deriveConfigured[from, to, path Concat ".OneOf"](configOfAtPath[to, flags, path Concat ".OneOf"](defaultDefinitionWithFlags))
                        .asInstanceOf[Transformer[From, To]]
                        .transform(from)
          }
        else
          findACase[From, To, fromLeft, toLeft, Position + 1](config)(from)
      case _: (EmptyTuple, EmptyTuple) => 
        throw new Exception("Should not be here, bug in implementation, report it")
      case _ =>
        inline config match
          case _: TypeDeriveConfig[_, _, path] =>
            MacroUtils.reportErrorAtPath(constValue[path], "Structure of transformed coproducts do not match")

  end findACase
end CoproductDerive