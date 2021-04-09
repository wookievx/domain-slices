package experimental.chimneylike.internal.derived

import experimental.chimneylike._
import experimental.chimneylike.dsl._
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal._

import scala.compiletime._
import scala.collection.Factory
import scala.collection.Map
import scala.collection.mutable.Builder

object SpecialDerive:
  import DeriveUtils._

  private given identity[T]: Transformer[T, T] with
    def transform(from: T): T = from

  //unlawfull instance to allow unifying option handling with collections, hack, but want to make it work
  private def optionInstance[T] = new Factory[T, Option[T]]:
    def fromSpecific(it: IterableOnce[T]): Option[T] = {
      val iter = it.iterator
      iter.nextOption()
    }
    def newBuilder: Builder[T, Option[T]] = new Builder[T, Option[T]]:
      private var res: Option[T] = None
      override def clear(): Unit = res = None
      override def result(): Option[T] = res
      override def addOne(elem: T): this.type =
        res = Some(elem)
        this

  transparent inline def deriveSpecialCases[From, To, Flags <: Tuple, Path <: String]: Option[Transformer[From, To]] =
    inline erasedValue[From] match
      case _: Option[a] =>
        inline erasedValue[To] match
          case _: Option[b] =>
            Some(deriveCollection[a, b, Flags, Path](using optionInstance[b].asInstanceOf).asInstanceOf[Transformer[From, To]])
          case _ =>
            None
      case _: Map[ka, a] =>
        inline erasedValue[To] match
          case _: Map[kb, b] =>
            summonFrom {
              case factory: Factory[(kb, b), To] =>
                Some(deriveMap[ka, kb, a, b, Flags, Path](using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive map instance, probably a library bug, encountered at: ")
            }
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
      case _: Option[a] =>
        inline erasedValue[To] match
          case _: Option[b] =>
            Some(deriveCollection[a, b, Flags, Path](using optionInstance[b].asInstanceOf).asInstanceOf[Transformer[From, To]])
          case _: F[Option[b]] =>
            Some(deriveCollectionF[F, a, b, Flags, Path](using optionInstance[b].asInstanceOf).asInstanceOf[TransformerF[F, From, To]])
          case _ =>
            None
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
    new SpecialDerive[IterableOnce[A], Any]({ from =>
      val builder = summon[Factory[B, Any]].newBuilder
      for elem <- from.iterator do builder += elemTransform.transform(elem)
      builder.result
    })

  inline def deriveMap[KA, KB, A, B, Flags <: Tuple, Path <: String](using Factory[(KB, B), Map[KB, B]]): Transformer[Map[KA, A], Map[KB, B]] =
    val keyTansform = summonFrom {
      case inst: Transformer[KA, KB] => inst
      case _ =>
        TransformerDerive.deriveConfigured[KA, KB, Concat[Path, "{*:}"]](configOfAtPath[KB, Flags, Concat[Path, "{*:}"]](defaultDefinitionWithFlags))
    }
    val valueTransform = summonFrom {
      case inst: Transformer[A, B] => inst
      case _ =>
        TransformerDerive.deriveConfigured[A, B, Concat[Path, "{:*}"]](configOfAtPath[B, Flags, Concat[Path, "{:*}"]](defaultDefinitionWithFlags))
    }
    new SpecialDerive[Map[KA, A], Map[KB, B]]({ from =>
      val builder = summon[Factory[(KB, B), Map[KB, B]]].newBuilder
      for (k, v) <- from.iterator do builder.addOne(keyTansform.transform(k) -> valueTransform.transform(v))
      builder.result
    })
  end deriveMap

  inline def deriveCollectionF[F[_], A, B, Flags <: Tuple, Path <: String](using Factory[B, _], TransformerFSupport[F]): TransformerF[F, IterableOnce[A], Any] = 
    val elemTransform = summonFrom {
      case inst: TransformerF[F, A, B] => inst
      case _ => 
        TransformerDerive.deriveConfiguredF[F, A, B, Concat[Path, "F[[*]]"]](configOfAtPath[B, Flags, Concat[Path, "F[[*]]"]](defaultDefinitionWithFlags))
    }
    new SpecialDeriveF[F, IterableOnce[A], Any](from => summon[TransformerFSupport[F]].traverse(from.iterator, elemTransform.transform))
  

  inline def deriveSupport[F[_], From, To, Flags <: Tuple, Path <: String](using TransformerFSupport[F]): Transformer[F[From], F[To]] =
    val underlyingTransform = summonFrom {
      case inst: Transformer[From, To] => inst
      case _ =>
        TransformerDerive.deriveConfigured[From, To, Concat[Path, "F[*]"]](configOfAtPath[To, Flags, Concat[Path, "F[*]"]](defaultDefinitionWithFlags))
    }
    new SpecialDerive[F[From], F[To]](summon[TransformerFSupport[F]].map(_, underlyingTransform.transform))


  class SpecialDerive[From, To](impl: From => To) extends Transformer[From, To]:
    def transform(from: From): To = impl(from)

  class SpecialDeriveF[F[_], From, To](impl: From => F[To]) extends TransformerF[F, From, To]:
    def transform(from: From): F[To] = impl(from)
end SpecialDerive