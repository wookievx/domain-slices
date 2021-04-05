package experimental.chimneylike.internal.derived

import experimental.chimneylike.*
import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*
import scala.compiletime.ops.int.*
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

object TransformerDerive:
  import DeriveUtils.*
  import TransformerCfg.*

  inline def derived[From, To, Config <: Tuple, Flags <: Tuple](config: TransformerDefinition[From, To, Config, Flags]): Transformer[From, To] =
    summonFrom {
      case tm: Mirror.ProductOf[To] =>
        summonFrom {
          case fm: Mirror.ProductOf[From] =>
            new Transformer[From, To]:
              def transform(from: From): To =
                val input = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
                val output = new Array[Any](constValue[Tuple.Size[tm.MirroredElemTypes]])
                val genConfig = configOf(config)
                DeriveProduct.handleTargetImpl[From, To, tm.MirroredElemTypes, tm.MirroredElemLabels, 0](genConfig, fm)(output.asInstanceOf, input.asInstanceOf, from)
                tm.fromProduct(Tuple.fromArray(output))
          case _ =>
            error("Automatic derivation not supported (yet) for supplied types")
        }
      case _ =>
        error("Automatic derivation not supported (yet) for supplied types")
    }
  end derived

  inline def derived[F[_], From, To, Config <: Tuple, Flags <: Tuple](config: TransformerFDefinition[F, From, To, Config, Flags])(using sup: TransformerFSupport[F]): TransformerF[F, From, To] =
    summonFrom {
      case tm: Mirror.ProductOf[To] =>
        summonFrom {
          case fm: Mirror.ProductOf[From] =>
            new TransformerF[F, From, To]:
              def transform(from: From): F[To] =
                val input = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
                val output = sup.pure(new Array[Any](constValue[Tuple.Size[tm.MirroredElemTypes]]))
                val genConfig = configOf(config)
                val finalOutput = DeriveProduct.handleTargetWithFImpl[F, From, To, tm.MirroredElemTypes, tm.MirroredElemLabels, 0](genConfig, fm)(output.asInstanceOf, input.asInstanceOf, from)
                sup.map(finalOutput, output => tm.fromProduct(Tuple.fromArray(output)))
          case _ =>
            error("Automatic derivation not supported (yet) for supplied types")
        }
      case _ =>
        error("Automatic derivation not supported (yet) for supplied types")
    }
  end derived
end TransformerDerive

object DeriveProduct:

  import DeriveUtils.*
  import TransformerCfg.*

  inline def handleTargetImpl[From, To, ToValues <: Tuple, TLabels <: Tuple, AtPos <: Int](
    inline config: ClassProductConfig[_, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: Array[Any],
    inputArray: IArray[Any],
    input: From
  ): Unit = 
    inline (erasedValue[ToValues], erasedValue[TLabels]) match
      case _: ((fieldType *: tailValues), (field *: tailLabels)) =>
        handleTargetField[AtPos, field, fieldType, From, To](config, fm)(outputArray, inputArray, input)
        handleTargetImpl[From, To, tailValues, tailLabels, AtPos + 1](config, fm)(outputArray, inputArray, input)
      case _ =>
  end handleTargetImpl

  inline def handleTargetWithFImpl[F[_], From, To, ToValues <: Tuple, TLabels <: Tuple, AtPos <: Int](
    inline config: ClassProductConfig[_, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: F[Array[Any]],
    inputArray: IArray[Any],
    input: From
  )(using TransformerFSupport[F]): F[Array[Any]] = 
    inline (erasedValue[ToValues], erasedValue[TLabels]) match
      case _: ((fieldType *: tailValues), (field *: tailLabels)) =>
        handleTargetWithFImpl[F, From, To, tailValues, tailLabels, AtPos + 1](
          config, 
          fm
        )(
          handleTargetFieldWithF[F, AtPos, field, fieldType, From, To](config, fm)(outputArray, inputArray, input), 
          inputArray, 
          input
        )
      case _ =>
        outputArray
  end handleTargetWithFImpl

  inline def handleTargetField[At <: Int, Name, T, From, To](
    inline config: ClassProductConfig[_, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: Array[Any],
    inputArray: IArray[Any],
    input: From
  ): Unit = 
    inline config match
      case config: ClassProductConfig[config, flags] =>
        inline erasedValue[ConfigOf[config, Name]] match
          case _: FieldConst[_] => outputArray(constValue[At]) = config.overrides(constValue[Name].asInstanceOf)
          case _: FieldComputed[_] => 
            val f = config.overrides(constValue[Name].asInstanceOf).asInstanceOf[From => Any]
            outputArray(constValue[At]) = f(input)
          case _ => 
            extractFromSource[At, Name, T, From, To, flags](config, fm)(outputArray, inputArray)
  end handleTargetField       

  inline def handleTargetFieldWithF[F[_], At <: Int, Name, T, From, To](
    inline config: ClassProductConfig[_, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: F[Array[Any]],
    inputArray: IArray[Any],
    input: From
  )(using sup: TransformerFSupport[F]): F[Array[Any]] = 
    inline config match
      case config: ClassProductConfig[config, flags] =>
        inline erasedValue[ConfigOf[config, Name]] match
          case _: FieldConst[_] =>
            sup.map(outputArray, { outputArray =>
              outputArray(constValue[At]) = config.overrides(constValue[Name].asInstanceOf)
              outputArray
            })
          case _: FieldConstF[_] =>
            sup.map(
              sup.product(outputArray, config.overrides(constValue[Name].asInstanceOf).asInstanceOf[F[Any]]), { (outputArray, value) =>
                outputArray(constValue[At]) = value
                outputArray
              }
            )
          case _: FieldComputed[_] =>
            sup.map(outputArray, { outputArray =>
              val f = config.overrides(constValue[Name].asInstanceOf).asInstanceOf[From => Any]
              outputArray(constValue[At]) = f(input)
              outputArray
            })
          case _: FieldComputedF[_] =>
            val f = config.overrides(constValue[Name].asInstanceOf).asInstanceOf[From => F[Any]]
            sup.map(
              sup.product(outputArray, f(input)), { (outputArray, value) =>
                outputArray(constValue[At]) = value
                outputArray
              }  
            )
          case _ => 
            sup.map(outputArray, outputArray =>
              extractFromSource[At, Name, T, From, To, flags](config, fm)(outputArray, inputArray)
              outputArray
            )
  end handleTargetFieldWithF

  inline def extractFromSource[TargetAt <: Int, LabelAt, TypeAt, From, To, Flags <: Tuple](
    inline config: ClassProductConfig[_, Flags],
    fm: Mirror.ProductOf[From]
  )(outputArray: Array[Any], inputArray: IArray[Any]): Unit = 
    findInSource[From, To, LabelAt, fm.MirroredElemLabels, TypeAt, fm.MirroredElemTypes, 0](
      config)(outputArray, inputArray, constValue[TargetAt])
    

  inline def findInSource[From, To, Field, SourceFields <: Tuple, Tpe, SourceTypes <: Tuple, Pos <: Int](
    inline config: ClassProductConfig[_, _])(outputArray: Array[Any], inputArray: IArray[Any], targetPosition: Int): Unit = 
    inline config match
      case c: ClassProductConfig[_, flags] =>
        inline (erasedValue[SourceFields], erasedValue[SourceTypes]) match
          case _: (Field *: _, Tpe *: _) =>
            outputArray(targetPosition) = inputArray(constValue[Pos])
          case _: (Field *: _, tpe *: _) =>
            error("More advanced cases are not handled for now")
          case _: (_ *: sourceFields, _ *: sourceTypes) =>
            findInSource[From, To, Field, sourceFields, Tpe, sourceTypes, Pos + 1](config)(outputArray, inputArray, targetPosition)
          case _: (EmptyTuple, _) =>
            inline if constValue[HasAFlag[flags, TransformerFlag.DefaultValues]] then
              inline if MacroUtils.defaultValueExistsIn[To](constValue[Field]) then
                outputArray(targetPosition) = config.defaults(constValue[Field].asInstanceOf)
              else 
                error("Unable to find default value in target when its missing from source")
            else
              error("Failed to locate a field in source class")
  end findInSource

  
end DeriveProduct

object DeriveUtils:

  final class ClassProductConfig[Config <: Tuple, Labels <: Tuple](
    val overrides: Map[String, Any],
    val defaults: Map[String, Any]
  )

  inline def configOf[To, Config <: Tuple, Labels <: Tuple](
    definition: TransformerDefinition[_, To, Config, Labels]
  ): ClassProductConfig[Config, Labels] =
    ClassProductConfig(definition.overrides, MacroUtils.getDefaultParams[To])

  inline def configOf[To, Config <: Tuple, Labels <: Tuple](
    definition: TransformerFDefinition[_, _, To, Config, Labels]
  ): ClassProductConfig[Config, Labels] =
    ClassProductConfig(definition.overrides, MacroUtils.getDefaultParams[To])

  import TransformerCfg.*

  type ConfigOf[Config <: Tuple, Field] <: TransformerCfg = Config match
    case FieldConst[Field] *: _ => FieldConst[Field]
    case FieldConstF[Field] *: _ => FieldConstF[Field]
    case FieldComputed[Field] *: _ => FieldComputed[Field]
    case FieldComputedF[Field] *: _ => FieldComputedF[Field]
    case _ *: tail => ConfigOf[tail, Field]
    case EmptyTuple => TransformerCfg

  type HasAFlag[Labels <: Tuple, Flag <: TransformerFlag] <: Boolean = Labels match 
    case Flag *: _ => true
    case _ *: tail => HasAFlag[tail, Flag]
    case EmptyTuple => false    


end DeriveUtils