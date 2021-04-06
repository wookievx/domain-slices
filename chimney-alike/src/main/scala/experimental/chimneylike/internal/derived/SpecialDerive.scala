package experimental.chimneylike.internal.derived

import experimental.chimneylike.*
import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*
import scala.compiletime.*
import scala.collection.Factory

object SpecialDerive:
  import DeriveUtils.*

  private given identity[T]: Transformer[T, T] with
    def transform(from: T): T = from

  transparent inline def deriveSpecialCases[From, To, Flags <: Tuple, Path <: String]: Option[Transformer[From, To]] =
    inline erasedValue[From] match
      case _: IterableOnce[a] =>
        inline erasedValue[To] match
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(deriveCollection[a, b, Flags, Path](using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive collection instance, probably a library bug, encountered at: ")
            }
          case _ =>
            None
      case _ => None
      
  transparent inline def deriveSpecialCasesF[F[_], From, To, Flags <: Tuple, Path <: String](using TransformerFSupport[F]) =
    inline erasedValue[From] match
      case _: IterableOnce[a] =>
        inline erasedValue[To] match
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(deriveCollection[a, b, Flags, Path](using factory).asInstanceOf[Transformer[From, To]])
              case _ =>
                MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive collection instance, probably a library bug, encountered at: ")
            }
          case _: F[x] =>
            inline erasedValue[x] match
              case _: IterableOnce[b] =>
                summonFrom {
                  case factory: Factory[b, x] =>
                    Some(deriveCollectionF[F, a, b, Flags, Path](using factory).asInstanceOf[TransformerF[F, From, To]])
                  case _ =>
                    MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive collection instance, probably a library bug, encountered at: ")
                }
              case b =>
                None
      case _: F[x] =>
        inline erasedValue[To] match
          case _: F[y] =>
            Some(deriveSupport[F, x, y, Flags, Path])
          case _ =>
            None
      case _ =>
        inline erasedValue[To] match
          case _: F[y] =>
            Some(TransformerDerive.deriveConfiguredF[F, From, y, Concat[Path, "F[*]"]](configOfAtPath[y, Flags, Concat[Path, "F[*]"]](defaultDefinitionWithFlags)))
          case _ =>
            None


  inline def deriveCollection[A, B, Flags <: Tuple, Path <: String](using Factory[B, _]): Transformer[IterableOnce[A], Any] = 
    val elemTransform = summonFrom {
      case inst: Transformer[A, B] => inst
      case _ => 
        TransformerDerive.deriveConfigured[A, B, Concat[Path, "[*]"]](configOfAtPath[B, Flags, Concat[Path, "[*]"]](defaultDefinitionWithFlags))
    }
    new Transformer[IterableOnce[A], Any]:
      def transform(from: IterableOnce[A]): Any =
        val builder = summon[Factory[B, Any]].newBuilder
        for elem <- from.iterator do builder += elemTransform.transform(elem)
        builder.result

  inline def deriveCollectionF[F[_], A, B, Flags <: Tuple, Path <: String](using Factory[B, _], TransformerFSupport[F]): TransformerF[F, IterableOnce[A], Any] = 
    val elemTransform = summonFrom {
      case inst: TransformerF[F, A, B] => inst
      case _ => 
        TransformerDerive.deriveConfiguredF[F, A, B, Concat[Path, "F[[*]]"]](configOfAtPath[B, Flags, Concat[Path, "F[[*]]"]](defaultDefinitionWithFlags))
    }
    new TransformerF[F, IterableOnce[A], Any]:
      def transform(from: IterableOnce[A]): F[Any] =
        summon[TransformerFSupport[F]].traverse(from.iterator, elemTransform.transform)
  

  inline def deriveSupport[F[_], From, To, Flags <: Tuple, Path <: String](using TransformerFSupport[F]): Transformer[F[From], F[To]] =
    val underlyingTransform = summonFrom {
      case inst: Transformer[From, To] => inst
      case _ =>
        TransformerDerive.deriveConfigured[From, To, Concat[Path, "F[*]"]](configOfAtPath[To, Flags, Concat[Path, "F[*]"]](defaultDefinitionWithFlags))
    }
    new Transformer[F[From], F[To]]:
      def transform(from: F[From]): F[To] =
        summon[TransformerFSupport[F]].map(from, underlyingTransform.transform)

end SpecialDerive