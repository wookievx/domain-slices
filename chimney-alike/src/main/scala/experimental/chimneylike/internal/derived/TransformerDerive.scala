package experimental.chimneylike.internal.derived

import experimental.chimneylike._
import experimental.chimneylike.dsl._
import experimental.chimneylike.internal.utils._
import experimental.chimneylike.internal._

import scala.compiletime.ops.int._
import scala.compiletime._
import scala.deriving._
import scala.quoted._

object TransformerDerive:
  import DeriveUtils._
  import TransformerCfg._

  inline def derived[From, To, Config <: Tuple, Flags <: Tuple](config: TransformerDefinition[From, To, Config, Flags]): Transformer[From, To] =
    deriveConfigured[From, To, ""](configOf(config))

  inline def derived[F[_], From, To, Config <: Tuple, Flags <: Tuple](config: TransformerFDefinition[F, From, To, Config, Flags])(using sup: TransformerFSupport[F]): TransformerF[F, From, To] =
    deriveConfiguredF[F, From, To, ""](configOf(config))
 
  inline def deriveConfigured[From, To, P <: String](inline config: TypeDeriveConfig[_, _, P]): Transformer[From, To] =
    summonFrom {
      case fm: Mirror.ProductOf[From] =>
        summonFrom {
          case tm: Mirror.ProductOf[To] =>
            ProductTransformer[From, To](
              constValue[Tuple.Size[tm.MirroredElemTypes]],
              tm,
              (output: Array[Any], input: IArray[Any], from: From) =>
                DeriveProduct.handleTargetImpl[From, To, tm.MirroredElemTypes, tm.MirroredElemLabels, 0](config, fm)(output.asInstanceOf, input.asInstanceOf, from)
            )
          case _ =>
            MacroUtils.reportErrorAtPath[P](constValue[P], "Automatic derivation not supported (yet) for supplied types")
        }
      case fm: Mirror.SumOf[From] =>
        summonFrom {
          case tm: Mirror.SumOf[To] =>
            CoproductDerive.derived[From, To, P](config)(using fm, tm)
          case _ =>
            MacroUtils.reportErrorAtPath[P](constValue[P], "Automatic derivation not supported (yet) for supplied types")
        }
      case _ =>
        MacroUtils.reportErrorAtPath[P](constValue[P], "Automatic derivation not supported (yet) for supplied types")
    }

  end deriveConfigured

  inline def deriveConfiguredF[F[_], From, To, P <: String](inline
   config: TypeDeriveConfig[_, _, P]
  )(using sup: TransformerFSupport[F]): TransformerF[F, From, To] = 
    summonFrom {
      case fm: Mirror.ProductOf[From] =>
        summonFrom {
          case tm: Mirror.ProductOf[To] =>
            new TransformerF[F, From, To]:
              def transform(from: From): F[To] =
                val input = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
                val output = sup.pure(new Array[Any](constValue[Tuple.Size[tm.MirroredElemTypes]]))
                DeriveProduct.handleTargetWithFImpl[F, From, To, tm.MirroredElemTypes, tm.MirroredElemLabels, 0](config, fm)(output.asInstanceOf, input.asInstanceOf, from)
                sup.map(output, output => tm.fromProduct(ArrayProduct(output)))
          case _ =>
            MacroUtils.reportErrorAtPath[P](constValue[P], "Automatic derivation not supported (yet) for supplied types")
        }
      case _ =>
        MacroUtils.reportErrorAtPath[P](constValue[P], "Automatic derivation not supported (yet) for supplied types")
    }

  class ProductTransformer[From, To](
    size: Int,
    tm: Mirror.ProductOf[To],
    productTransform: (Array[Any], IArray[Any], From) => Unit
  ) extends Transformer[From, To]:
    def transform(from: From): To =
      val input = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
      val output = new Array[Any](size)
      productTransform(output, input, from)
      tm.fromProduct(ArrayProduct(output))

end TransformerDerive

object DeriveProduct:

  import DeriveUtils._
  import TransformerCfg._

  inline def handleTargetImpl[From, To, ToValues <: Tuple, TLabels <: Tuple, AtPos <: Int](
    inline config: TypeDeriveConfig[_, _, _],
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
    inline config: TypeDeriveConfig[_, _, _],
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
    inline config: TypeDeriveConfig[_, _, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: Array[Any],
    inputArray: IArray[Any],
    input: From
  ): Unit = 
    inline config match
      case config: TypeDeriveConfig[config, flags, _] =>
        inline erasedValue[ConfigOf[config, Name]] match
          case _: FieldConst[_] => outputArray(constValue[At]) = config.overrides(constValue[Name].asInstanceOf)
          case _: FieldComputed[_] => 
            val f = config.overrides(constValue[Name].asInstanceOf).asInstanceOf[From => Any]
            outputArray(constValue[At]) = f(input)
          case _: FieldRelabelled[fieldFrom, Name] =>
            extractFromSource[At, fieldFrom, T, From, To, flags](config, fm)(outputArray, inputArray)
          case _ => 
            extractFromSource[At, Name, T, From, To, flags](config, fm)(outputArray, inputArray)
  end handleTargetField       

  inline def handleTargetFieldWithF[F[_], At <: Int, Name, T, From, To](
    inline config: TypeDeriveConfig[_, _, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: F[Array[Any]],
    inputArray: IArray[Any],
    input: From
  )(using sup: TransformerFSupport[F]): F[Array[Any]] = 
    inline config match
      case config: TypeDeriveConfig[config, flags, _] =>
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
          case _: FieldRelabelled[fieldFrom, Name] =>
            sup.map(outputArray, outputArray =>
              extractFromSource[At, fieldFrom, T, From, To, flags](config, fm)(outputArray, inputArray)
              outputArray
            )
          case _ => 
            sup.map(outputArray, outputArray =>
              extractFromSource[At, Name, T, From, To, flags](config, fm)(outputArray, inputArray)
              outputArray
            )
  end handleTargetFieldWithF

  inline def extractFromSource[TargetAt <: Int, LabelAt, TypeAt, From, To, Flags <: Tuple](
    inline config: TypeDeriveConfig[_, Flags, _],
    fm: Mirror.ProductOf[From]
  )(outputArray: Array[Any], inputArray: IArray[Any]): Unit = 
    findInSource[From, To, LabelAt, fm.MirroredElemLabels, TypeAt, fm.MirroredElemTypes, 0](
      config)(outputArray, inputArray, constValue[TargetAt])
    

  inline def findInSource[From, To, Field, SourceFields <: Tuple, Tpe, SourceTypes <: Tuple, Pos <: Int](
    inline config: TypeDeriveConfig[_, _, _])(outputArray: Array[Any], inputArray: IArray[Any], targetPosition: Int): Unit = 
    inline config match
      case c: TypeDeriveConfig[_, flags, path] =>
        inline (erasedValue[SourceFields], erasedValue[SourceTypes]) match
          case _: (Field *: _, Tpe *: _) =>
            outputArray(targetPosition) = inputArray(constValue[Pos])
          case _: (Field *: _, tpe *: _) =>
            inline MacroUtils.attemptSummonInstance[Transformer[tpe, Tpe]] match
              case Some(transformer: Transformer[tpe, Tpe]) =>
                val fixed = transformer.asInstanceOf[Transformer[Any, Tpe]]
                outputArray(targetPosition) = fixed.transform(inputArray(constValue[Pos]))
              case _ =>
                inline SpecialDerive.deriveSpecialCases[tpe, Tpe, flags, path Concat "." Concat Field] match
                  case Some(transformer) =>
                    outputArray(targetPosition) = transformer.asInstanceOf[Transformer[Any, Tpe]].transform(inputArray(constValue[Pos]))
                  case None =>
                    TransformerDerive.deriveConfigured[tpe, Tpe, path Concat "." Concat Field](
                      configOfAtPath[Tpe, TransformerFlag.DefaultValues *: EmptyTuple, path Concat "." Concat Field](defaultDefinitionWithFlags))
                      .asInstanceOf[Transformer[Any, Tpe]]
                      .transform(inputArray(constValue[Pos]))
          case _: (_ *: sourceFields, _ *: sourceTypes) =>
            findInSource[From, To, Field, sourceFields, Tpe, sourceTypes, Pos + 1](config)(outputArray, inputArray, targetPosition)
          case _: (EmptyTuple, _) =>
            inline if constValue[HasAFlag[flags, TransformerFlag.DefaultValues]] then
              inline if MacroUtils.defaultValueExistsIn[To](constValue[Field]) then
                outputArray(targetPosition) = config.defaults(constValue[Field].asInstanceOf)
              else
                MacroUtils.reportErrorAtPathWithType[path, Concat["missing default: ", Field], To]("Unable to find default value in target when its missing from source")
              else
              error(constValue["Failed to locate a field in source class, when deriving at" Concat path])
  end findInSource

  inline def findInSourceWithF[F[_], From, To, Field, SourceFields <: Tuple, Tpe, SourceTypes <: Tuple, Pos <: Int](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    outputArray: F[Array[Any]], 
    inputArray: IArray[Any], 
    targetPosition: Int
  )(using sup: TransformerFSupport[F]): Unit = 
    inline config match
      case c: TypeDeriveConfig[_, flags, path] =>
        inline (erasedValue[SourceFields], erasedValue[SourceTypes]) match
          case _: (Field *: _, Tpe *: _) =>
            sup.map(outputArray, outputArray => outputArray(targetPosition) = inputArray(constValue[Pos]))
          case _: (Field *: _, tpe *: _) =>
            summonFrom {
              case transformer: Transformer[tpe, Tpe] =>
                val fixed =  transformer.asInstanceOf[Transformer[Any, Tpe]]
                sup.map(outputArray, outputArray => 
                  outputArray(targetPosition) = fixed.transform(inputArray(constValue[Pos])))
              case transformerF: TransformerF[F, tpe, Tpe] =>
                val fixed = transformerF.asInstanceOf[TransformerF[F, Any, Tpe]]
                sup.map(
                  sup.product(outputArray, fixed.transform(inputArray(constValue[Pos]))),
                  (outputArray, value) => outputArray(targetPosition) = value
                )
              case _ =>
                inline SpecialDerive.deriveSpecialCasesF[F, tpe, Tpe, flags, path Concat "." Concat Field] match
                  case Some(transformer: Transformer[tpe, Tpe]) =>
                    sup.map(outputArray, outputArray =>
                      outputArray(targetPosition) = transformer.asInstanceOf[Transformer[Any, Tpe]].transform(inputArray(constValue[Pos])))
                  case Some(transformerF: TransformerF[F, tpe, Tpe]) =>
                    val fixed = transformerF.asInstanceOf[TransformerF[F, Any, Tpe]]
                    sup.map(
                      sup.product(outputArray, fixed.transform(inputArray(constValue[Pos]))),
                      (outputArray, value) => outputArray(targetPosition) = value
                    )
                  case _ =>
                    TransformerDerive.deriveConfigured[tpe, Tpe, Concat[path, path Concat "." Concat Field]](
                      configOfAtPath[Tpe, TransformerFlag.DefaultValues *: EmptyTuple, Concat[path, path Concat "." Concat Field]](defaultDefinitionWithFlags))
                      .asInstanceOf[Transformer[Any, Tpe]]
                      .transform(inputArray(constValue[Pos]))
            }
          case _: (_ *: sourceFields, _ *: sourceTypes) =>
            findInSourceWithF[F, From, To, Field, sourceFields, Tpe, sourceTypes, Pos + 1](config)(outputArray, inputArray, targetPosition)
          case _: (EmptyTuple, _) =>
            inline if constValue[HasAFlag[flags, TransformerFlag.DefaultValues]] then
              inline if MacroUtils.defaultValueExistsIn[To](constValue[Field]) then
                sup.map(outputArray, outputArray => outputArray(targetPosition) = config.defaults(constValue[Field].asInstanceOf))
              else 
                error(constValue["Unable to find default value in target when its missing from source, when deriving at: " Concat path])
            else
              error(constValue["Failed to locate a field in source class, when deriving at" Concat path])
  end findInSourceWithF

  
end DeriveProduct

object DeriveUtils:

  final class TypeDeriveConfig[Config <: Tuple, Labels <: Tuple, Path <: String](
    val overrides: Map[String, Any],
    val defaults: Map[String, Any]
  )

  inline def configOf[To, Config <: Tuple, Labels <: Tuple](
    definition: TransformerDefinition[_, To, Config, Labels]
  ): TypeDeriveConfig[Config, Labels, ""] =
    TypeDeriveConfig(definition.overrides, MacroUtils.getDefaultParams[To])

  inline def configOf[To, Config <: Tuple, Labels <: Tuple](
    definition: TransformerFDefinition[_, _, To, Config, Labels]
  ): TypeDeriveConfig[Config, Labels, ""] =
    TypeDeriveConfig(definition.overrides, MacroUtils.getDefaultParams[To])

  inline def configOfAtPath[To, Labels <: Tuple, Path <: String](
    definition: TransformerDefinition[_, To, EmptyTuple, Labels]
  ): TypeDeriveConfig[EmptyTuple, Labels, Path] =
    TypeDeriveConfig(definition.overrides, MacroUtils.getDefaultParams[To])

  import TransformerCfg._

  type ConfigOf[Config <: Tuple, Field] <: TransformerCfg = Config match
    case FieldConst[Field] *: _ => FieldConst[Field]
    case FieldConstF[Field] *: _ => FieldConstF[Field]
    case FieldComputed[Field] *: _ => FieldComputed[Field]
    case FieldComputedF[Field] *: _ => FieldComputedF[Field]
    case FieldRelabelled[fromField, Field] *: _ => FieldRelabelled[fromField, Field]
    case _ *: tail => ConfigOf[tail, Field]
    case EmptyTuple => TransformerCfg

  type HasAFlag[Labels <: Tuple, Flag <: TransformerFlag] <: Boolean = Labels match 
    case Flag *: _ => true
    case _ *: tail => HasAFlag[tail, Flag]
    case EmptyTuple => false

  import scala.compiletime.ops.string._

  type Concat[Path <: String, Field] <: String = Field match
    case String => Path + Field
    case _ => Path
      


end DeriveUtils
