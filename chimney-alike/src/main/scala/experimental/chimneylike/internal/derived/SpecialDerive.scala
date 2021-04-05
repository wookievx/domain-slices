package experimental.chimneylike.internal.derived

import experimental.chimneylike.*
import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*
import scala.compiletime.*
import scala.collection.Factory

object SpecialDerive:

  private given identity[T]: Transformer[T, T] with
    def transform(from: T): T = from

  inline given deriveCollection[Col[x] <: IterableOnce[x], F, T, Flags <: Tuple](using Factory[T, Col[T]]): Transformer[Col[F], Col[T]] = 
    val elemTransform = summonFrom {
      case inst: Transformer[F, T] => inst
      case _ => 
        TransformerDerive.derived[F, T, EmptyTuple, Flags](defaultDefinitionWithFlags)
    }
    new Transformer[Col[F], Col[T]]:
      def transform(from: Col[F]): Col[T] =
        val builder = summon[Factory[T, Col[T]]].newBuilder
        for elem <- from.iterator do builder += elemTransform.transform(elem)
        builder.result
  
  inline given deriveCollectionF[F[_], Col[x] <: IterableOnce[x], From, To, Flags <: Tuple](using Factory[To, Col[To]], TransformerFSupport[F]): TransformerF[F, Col[From], Col[To]] = 
    val elemTransform = summonFrom {
      case inst: TransformerF[F, From, To] => inst
      case _ => 
        TransformerDerive.derived(defaultDefinitionWithFlags[From, To, Flags].lift[F])
    }
    new TransformerF[F, Col[From], Col[To]]:
      def transform(from: Col[From]): F[Col[To]] =
        summon[TransformerFSupport[F]].traverse(from.iterator, elemTransform.transform)

  inline given deriveSupport[F[_], From, To](using TransformerFSupport[F]): Transformer[F[From], F[To]] =
    val underlyingTransform = summonFrom {
      case inst: Transformer[From, To] => inst
      case _ =>
        TransformerDerive.derived(defaultDefinitionWithFlags[From, To, TransformerFlag.DefaultValues *: EmptyTuple])
    }
    new Transformer[F[From], F[To]]:
      def transform(from: F[From]): F[To] =
        summon[TransformerFSupport[F]].map(from, underlyingTransform.transform)

end SpecialDerive