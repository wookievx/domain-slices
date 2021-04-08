package experimental.chimneylike.internal.derived

import experimental.chimneylike.*
import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*
import scala.compiletime.*
import scala.collection.Factory

object SpecialPatcherDerive:
  import DeriveUtils.*

  private given identity[T]: Patcher[T, T] with
    def patch(obj: T, patch: T): T = patch

  transparent inline def deriveSpecialCases[T, P, Config <: Tuple, Path <: String]: Option[Patcher[T, P]] =
    MacroUtils.printAtCompileTime["Attempting to derive special cases at: " Concat Path]
    inline erasedValue[P] match
      case _: IterableOnce[p] =>
        inline erasedValue[T] match
          case _: IterableOnce[t] =>
            summonFrom {
              case f: Factory[t, T] =>
                Some(deriveCollection[t, p, Config, Path](using f.asInstanceOf[Factory[t, IterableOnce[t]]]).asInstanceOf[Patcher[T, P]])
              case _ =>
                MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive collection instance, probably a library bug, encountered at: ")
            }
          case _ =>
            None
      case _ => None

  inline def deriveCollection[T, P, Config <: Tuple, Path <: String](using Factory[T, IterableOnce[T]]): Patcher[IterableOnce[T], IterableOnce[P]] =
    val elemPatcher: Patcher[T, P] = summonFrom {
      case inst: Patcher[T, P] => inst
      case _ =>
        PatcherDerive.derived[T, P, Config, Path Concat "[*]"]
    }
    new Patcher[IterableOnce[T], IterableOnce[P]]:
      def patch(obj: IterableOnce[T], patch: IterableOnce[P]): IterableOnce[T] =
        val builder = summon[Factory[T, _]].newBuilder
        inline if canOverride[T, P, Config] then
          for p <- patch.iterator do builder += p.asInstanceOf[T]
          builder.result
        else
          obj.iterator.map(Some(_)).zipAll(patch.iterator.map(Some(_)), None, None)
            .collect { case (Some(obj), optP) => obj -> optP }
            .foreach { (obj, optP) =>
              optP match {
                case Some(optP) => builder += elemPatcher.patch(obj, optP)
                case None => builder += obj
              }
            }
          builder.result
  end deriveCollection

  private inline def canOverride[A, B, Config <: Tuple]: Boolean =
    inline erasedValue[A] match
      case _: B => constValue[HasPatcherCfg[Config, PatcherCfg.OverwriteIterablesOnTheSameType]]
      case _ => false

end SpecialPatcherDerive