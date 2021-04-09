package experimental.chimneylike.internal.derived

import experimental.chimneylike.*
import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils.*
import experimental.chimneylike.internal.utils.ArrayProduct
import experimental.chimneylike.internal.*

import scala.compiletime.ops.int.*
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*


object PatcherDeriveProduct:
  import DeriveUtils.Concat

  inline def deriveProduct[T, P, Config <: Tuple, Path <: String](using tm: Mirror.ProductOf[T], pm: Mirror.ProductOf[P]): Patcher[T, P] = {
    printAtCompileTime["Deriving product at: " Concat Path]
    new Patcher[T, P]:
      def patch(obj: T, patch: P): T =
        val input = Tuple.fromProduct(patch.asInstanceOf[Product]).toIArray
        val output = Tuple.fromProduct(obj.asInstanceOf[Product]).toArray
        applyFields[pm.MirroredElemLabels, pm.MirroredElemTypes, tm.MirroredElemLabels, tm.MirroredElemTypes, Config, Path, T](
          patchPosition = 0,
          targetPosition = 0
        )(
          input,
          output.asInstanceOf[Array[Any]]
        )
        tm.fromProduct(ArrayProduct(output))
  }

  inline def deriveProductN[T, P <: Tuple, Config <: Tuple](using tm: Mirror.ProductOf[T]): Patcher[T, P] =
    printAtCompileTime["Deriving product of n patchers"]
    new Patcher[T, P]:
      def patch(obj: T, patch: P): T =
        val output = Tuple.fromProduct(obj.asInstanceOf[Product]).toArray
        deriveProducNImpl[T, P, Config, 0](output.asInstanceOf, patch)
        tm.fromProduct(ArrayProduct(output))
  end deriveProductN

  private[derived] inline def deriveProducNImpl[T, P <: Tuple, Config <: Tuple, Pos <: Int](target: Array[Any], patch: P)(using tm: Mirror.ProductOf[T]): Unit =
    inline erasedValue[P] match
      case _: (p *: patches) =>
        showType[p]
        inline summonProductOf[p] match
          case pm: Mirror.ProductOf[p] =>
            deriveInplace[T, p, Config, Pos](using tm, pm).patch(target, patch.asInstanceOf[NonEmptyTuple].head.asInstanceOf[p])
          case _ =>
            error("WTF")
        deriveProducNImpl[T, patches, Config, Pos + 1](target, patch.asInstanceOf[NonEmptyTuple].tail.asInstanceOf[patches])
      case _: EmptyTuple =>
  end deriveProducNImpl

  private[derived] inline def deriveInplace[T, P, Config <: Tuple, Pos <: Int](using tm: Mirror.ProductOf[T], pm: Mirror.ProductOf[P]): Patcher[Array[Any], P] =
    printAtCompileTime[Concat["Deriving inplace patcher at: ", ToString[Pos]]]
    new Patcher[Array[Any], P]:
      def patch(obj: Array[Any], patch: P): Array[Any] =
        val input = Tuple.fromProduct(patch.asInstanceOf[Product]).toIArray
        applyFields[pm.MirroredElemLabels, pm.MirroredElemTypes, tm.MirroredElemLabels, tm.MirroredElemTypes, Config, "", T](
          patchPosition = 0,
          targetPosition = 0
        )(
          input,
          obj
        )
        obj
  end deriveInplace

  inline def applyFields[PFields <: Tuple, PTypes <: Tuple, TFields <: Tuple, TTypes <: Tuple, Config <: Tuple, Path <: String, T](
    patchPosition: Int,
    targetPosition: Int
  )(
    patch: IArray[Any],
    target: Array[Any]
  ): Unit =
    inline (erasedValue[PFields], erasedValue[PTypes]) match
      case _: (pField *: pFields, pType *: pTypes) =>
        printAtCompileTime["Looking up: " Concat pField Concat " at " Concat Path]
        applyField[pField, pType, TFields, TTypes, Config, Path Concat "." Concat pField, T](
          patchPosition = patchPosition,
          targetPosition = targetPosition
        )(
          patch = patch,
          target = target
        )
        applyFields[pFields, pTypes, TFields, TTypes, Config, Path, T](
          patchPosition = patchPosition + 1,
          targetPosition = targetPosition
        )(
          patch = patch,
          target = target
        )
      case _ =>


  private inline def applyField[PField, PType, TFields <: Tuple, TTypes <: Tuple, Config <: Tuple, Path <: String, T](
    patchPosition: Int,
    targetPosition: Int
  )(
    patch: IArray[Any],
    target: Array[Any]
  ): Unit =
    inline (erasedValue[TFields], erasedValue[TTypes]) match
      case _: (PField *: _, Option[tType] *: _) =>
        printAtCompileTime["Having optional target: " Concat PField Concat " at " Concat Path]
        handleOptionTarget[tType, PType, Config, Path](patchPosition, targetPosition)(patch, target)
      case _: (PField *: _, tType *: _) =>
        printAtCompileTime["Having regular target: " Concat PField Concat " at " Concat Path]
        handleOtherTarget[tType, PType, Config, Path](patchPosition, targetPosition)(patch, target)
      case _: (_ *: tfields, _ *: ttypes) =>
        applyField[PField, PType, tfields, ttypes, Config, Path, T](
          patchPosition = patchPosition,
          targetPosition = targetPosition + 1
        )(
          patch,
          target
        )
      case _: (EmptyTuple, _) =>
        inline if constValue[HasPatcherCfg[Config, PatcherCfg.IgnoreRedundantPatcherFields]] then {}
        else reportErrorAtPathWithType[Path, Path, T]("Derivation failed because target type is missing a field")

  private inline def handleOptionTarget[TType, PType, Config <: Tuple, Path <: String](
    patchPosition: Int,
    targetPosition: Int
  )(
    patch: IArray[Any],
    target: Array[Any]
  ): Unit = {
    inline erasedValue[PType] match
      case _: Option[ptype] =>
        patch(patchPosition).asInstanceOf[Option[ptype]] match
          case Some(patchValue) =>
            target(targetPosition) = Some(handleUnpackedTypes[TType, ptype, Config, Path](patchValue, target(targetPosition).asInstanceOf[TType]))
          case None =>
            inline if constValue[HasPatcherCfg[Config, PatcherCfg.IgnoreNoneInPatch]] then
              {}
            else
              target(targetPosition) = None
      case _ =>
        target(targetPosition) = Some(handleUnpackedTypes[TType, PType, Config, Path](patch(patchPosition).asInstanceOf[PType], target(targetPosition).asInstanceOf[TType]))
  }

  private inline def handleOtherTarget[TType, PType, Config <: Tuple, Path <: String](
    patchPosition: Int,
    targetPosition: Int
  )(
    patch: IArray[Any],
    target: Array[Any]
  ): Unit =
    inline erasedValue[PType] match
      case _: Option[ptype] =>
        patch(patchPosition).asInstanceOf[Option[ptype]] match
          case Some(patchValue) =>
            target(targetPosition) = handleUnpackedTypes[TType, ptype, Config, Path](patchValue, target(targetPosition).asInstanceOf[TType])
          case None =>
      case _ =>
        target(targetPosition) = handleUnpackedTypes[TType, PType, Config, Path](patch(patchPosition).asInstanceOf[PType], target(targetPosition).asInstanceOf[TType])

  private inline def handleUnpackedTypes[TType, PType, Config <: Tuple, Path <: String](
    patchValue: PType,
    targetValue: TType
  ): Any =
    inline erasedValue[PType] match
      case _: TType =>
        printAtCompileTime["Applying value at: " Concat Path]
        patchValue
      case _ =>
        summonFrom {
          case p: Patcher[TType, PType] =>
          case _ =>
            inline SpecialPatcherDerive.deriveSpecialCases[TType, PType, Config, Path] match
              case Some(p: Patcher[TType, PType]) =>
                printAtCompileTime["Used special cases at: " Concat Path]
                p.patch(targetValue, patchValue)
              case None =>
                printAtCompileTime["Unable to use special cases at" Concat Path]
                PatcherDerive.derived[TType, PType, Config, Path].patch(targetValue, patchValue)
        }

end PatcherDeriveProduct

object PatcherDeriveCoproduct:
  import DeriveUtils.Concat

  inline def deriveCoproduct[T, P, Config <: Tuple, Path <: String](using tm: Mirror.SumOf[T], pm: Mirror.SumOf[P]): Patcher[T, P] =
    printAtCompileTime["Deriving coproduct at: " Concat Path]
    new Patcher[T, P]:
      def patch(obj: T, patch: P): T =
        findACase[T, P, tm.MirroredElemTypes, pm.MirroredElemTypes, pm.MirroredElemLabels, 0, Config, Path](obj, patch)

  private inline def findACase[T, P, TLeft <: Tuple, PLeft <: Tuple, Labels <: Tuple, Position <: Int, Config <: Tuple, Path <: String](obj: T, patch: P)(using pm: Mirror.SumOf[P]): T =
    inline (erasedValue[TLeft], erasedValue[PLeft], erasedValue[Labels]) match
      case _: (t *: tLeft, p *: pLeft, pName *: labels) =>
        if pm.ordinal(patch) == constValue[Position] then
          inline SpecialPatcherDerive.deriveSpecialCases[t, p, Config, Path Concat "." Concat pName] match
            case Some(t: Patcher[t, p]) =>
              t.asInstanceOf[Patcher[T, P]].patch(obj, patch)
            case None =>
              PatcherDerive.derived[t, p, Config, Path Concat "." Concat pName]
                .asInstanceOf[Patcher[T, P]]
                .patch(obj, patch)
        else
          findACase[T, P, tLeft, pLeft, labels, Position + 1, Config, Path](obj, patch)
      case _: (EmptyTuple, EmptyTuple, _) =>
        throw new Exception("Should not be here, bug in implementation, report it")
      case _ =>
        error(constValue["Structured of patch and target coproducts do not match, encountered at: " Concat Path])

end PatcherDeriveCoproduct