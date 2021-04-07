package experimental.chimneylike.internal.dsl

import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*

import scala.quoted.{given, *}
import deriving.*, compiletime.*

object TransformerDefinitionBuilder:
  import GenericTransformerDefinitionBuilder.TransformerDefinitionBuilder

  transparent inline def withFieldConst[From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerDefinition[From, To, Config, Flags]
  )(inline 
    selector: To => T, 
    value: T
  ) = ${withFieldConstImpl('definition)('selector, 'value)}

  transparent inline def withFieldComputed[From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerDefinition[From, To, Config, Flags]
  )(inline 
    selector: To => T, 
    compute: From => T
  ) = ${ withFieldComputedImpl('definition)('selector, 'compute) }

  transparent inline def withFieldRelabelled[From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerDefinition[From, To, Config, Flags]
  )(inline 
    renameFrom: From => T, 
    inline
    renameTo: To => T
  ) = ${ withFieldRelabelledImpl('definition)('renameFrom, 'renameTo) }

  def withFieldConstImpl[From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerDefinition[From, To, Config, Flags]]
  )(
    selector: Expr[To => T], 
    value: Expr[T]
  )(using Quotes): Expr[Any] = {
    GenericTransformerDefinitionBuilder.withFieldConstImpl[[Config <: Tuple] =>> TransformerDefinition[From, To, Config, Flags], To, Config, T](
      definition,
      new TransformerDefinitionBuilder[From, To, Flags]
    )(
      selector, 
      value
    )
  }

  def withFieldComputedImpl[From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerDefinition[From, To, Config, Flags]]
  )(
    selector: Expr[To => T], 
    compute: Expr[From => T]
  )(using Quotes): Expr[Any] = {
    GenericTransformerDefinitionBuilder.withFieldComputedImpl[[Config <: Tuple] =>> TransformerDefinition[From, To, Config, Flags], From, To, Config, T](
      definition,
      new TransformerDefinitionBuilder[From, To, Flags]
    )(
      selector, 
      compute
    )
  }

  def withFieldRelabelledImpl[From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerDefinition[From, To, Config, Flags]]
  )(
    renameFrom: Expr[From => T], 
    renameTo: Expr[To => T]
  )(using Quotes): Expr[Any] =
    GenericTransformerDefinitionBuilder.withFieldRenamedImpl[[Config <: Tuple] =>> TransformerDefinition[From, To, Config, Flags], From, To, Config, Flags, T](
      definition,
      new TransformerDefinitionBuilder[From, To, Flags] 
    )(
      renameFrom,
      renameTo
    )

end TransformerDefinitionBuilder

object TransformerFDefinitionBuilder:
  import GenericTransformerDefinitionBuilder.TransformerFDefinitionBuilder

  transparent inline def withFieldConst[F[_], From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerFDefinition[F, From, To, Config, Flags]
  )(inline 
    selector: To => T, 
    value: T
  ) = ${ withFieldConstImpl('definition)('selector, 'value) }

  transparent inline def withFieldConstF[F[_], From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerFDefinition[F, From, To, Config, Flags]
  )(inline 
    selector: To => T, 
    value: F[T]
  ) = ${ withFieldConstFImpl('definition)('selector, 'value) }

  transparent inline def withFieldComputed[F[_], From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerFDefinition[F, From, To, Config, Flags]
  )(inline 
    selector: To => T, 
    compute: From => T
  ) = ${ withFieldComputedImpl('definition)('selector, 'compute) }

  transparent inline def withFieldComputedF[F[_], From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerFDefinition[F, From, To, Config, Flags]
  )(inline 
    selector: To => T, 
    compute: From => F[T]
  ) = ${ withFieldComputedFImpl('definition)('selector, 'compute) }

  transparent inline def withFieldRelabelled[F[_], From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerFDefinition[F, From, To, Config, Flags]
  )(inline 
    renameFrom: From => T, 
    inline
    renameTo: To => T
  ) = ${ withFieldRelabelledImpl('definition)('renameFrom, 'renameTo) }

  def withFieldConstImpl[F[_]: Type, From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerFDefinition[F, From, To, Config, Flags]]
  )(
    selector: Expr[To => T], 
    value: Expr[T]
  )(using Quotes) = {
    GenericTransformerDefinitionBuilder.withFieldConstImpl[[Config <: Tuple] =>> TransformerFDefinition[F, From, To, Config, Flags], To, Config, T](
      definition,
      new TransformerFDefinitionBuilder[F, From, To, Flags]
    )(
      selector, 
      value
    )
  }

  def withFieldConstFImpl[F[_]: Type, From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerFDefinition[F, From, To, Config, Flags]]
  )(
    selector: Expr[To => T], 
    value: Expr[F[T]]
  )(using Quotes): Expr[Any] = {
    GenericTransformerDefinitionBuilder.withFieldConstFImpl[[Config <: Tuple] =>> TransformerFDefinition[F, From, To, Config, Flags], To, Config, T, F](
      definition,
      new TransformerFDefinitionBuilder[F, From, To, Flags]
    )(
      selector, 
      value
    )
  }

  def withFieldComputedImpl[F[_]: Type, From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerFDefinition[F, From, To, Config, Flags]]
  )(
    selector: Expr[To => T], 
    compute: Expr[From => T]
  )(using Quotes): Expr[Any] = {
    GenericTransformerDefinitionBuilder.withFieldComputedImpl[[Config <: Tuple] =>> TransformerFDefinition[F, From, To, Config, Flags], From, To, Config, T](
      definition,
      new TransformerFDefinitionBuilder[F, From, To, Flags]
    )(
      selector, 
      compute
    )
  }

  def withFieldComputedFImpl[F[_]: Type, From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerFDefinition[F, From, To, Config, Flags]]
  )(
    selector: Expr[To => T], 
    compute: Expr[From => F[T]]
  )(using Quotes): Expr[Any] = {
    GenericTransformerDefinitionBuilder.withFieldComputedFImpl[[Config <: Tuple] =>> TransformerFDefinition[F, From, To, Config, Flags], From, To, Config, T, F](
      definition,
      new TransformerFDefinitionBuilder[F, From, To, Flags]
    )(
      selector, 
      compute
    )
  }

  def withFieldRelabelledImpl[F[_]: Type, From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerFDefinition[F, From, To, Config, Flags]]
  )(
    renameFrom: Expr[From => T], 
    renameTo: Expr[To => T]
  )(using Quotes): Expr[Any] =
    GenericTransformerDefinitionBuilder.withFieldRenamedImpl[[Config <: Tuple] =>> TransformerFDefinition[F, From, To, Config, Flags], From, To, Config, Flags, T](
      definition,
      new TransformerFDefinitionBuilder[F, From, To, Flags] 
    )(
      renameFrom,
      renameTo
    )

end TransformerFDefinitionBuilder

object GenericTransformerDefinitionBuilder:

  def withFieldConstImpl[Definition[_ <: Tuple], To: Type, Config <: Tuple: Type, T: Type](
    definition: Expr[Definition[Config]],
    builder: FinalDefinitionBuilder[Definition]
  )(
    selector: Expr[To => T], 
    value: Expr[T]
  )(using quotes: Quotes): Expr[Any] = 
    import quotes.reflect.report
    val name = MacroUtils.extractNameFromSelectorImpl(selector)
    withFieldGenImpl[To, T](
      new ExprModifier:
        def apply[T <: String: Type](nameExpr: Expr[T]): Expr[Any] =
          builder.withField[Config, EnableConfig[Config, TransformerCfg.FieldConst[T]]](definition, name, value)
    )(selector)
  end withFieldConstImpl

  def withFieldConstFImpl[Definition[_ <: Tuple], To: Type, Config <: Tuple: Type, T: Type, F[_]: Type](
    definition: Expr[Definition[Config]], 
    builder: FinalDefinitionBuilder[Definition]
  )(
    selector: Expr[To => T],
    value: Expr[F[T]]
  )(using quotes: Quotes): Expr[Any] = 
    withFieldGenImpl[To, T](
      new ExprModifier:
        def apply[T <: String: Type](nameExpr: Expr[T]): Expr[Any] =
          builder.withField[Config, EnableConfig[Config, TransformerCfg.FieldConstF[T]]](definition, nameExpr, value)
    )(selector)
  end withFieldConstFImpl 

  def withFieldComputedImpl[Definition[_ <: Tuple], From: Type, To: Type, Config <: Tuple: Type, T: Type](
    definition: Expr[Definition[Config]],
    builder: FinalDefinitionBuilder[Definition]
  )(
    selector: Expr[To => T],
    compute: Expr[From => T]
  )(using quotes: Quotes): Expr[Any] =
    withFieldGenImpl[To, T](
      new ExprModifier:
        def apply[T <: String: Type](nameExpr: Expr[T]): Expr[Any] =
          builder.withField[Config, EnableConfig[Config, TransformerCfg.FieldComputed[T]]](definition, nameExpr, compute)
    )(selector)

  def withFieldComputedFImpl[Definition[_ <: Tuple], From: Type, To: Type, Config <: Tuple: Type, T: Type, F[_]: Type](
    definition: Expr[Definition[Config]],
    builder: FinalDefinitionBuilder[Definition]
  )(
    selector: Expr[To => T],
    compute: Expr[From => F[T]]
  )(using quotes: Quotes): Expr[Any] =
    withFieldGenImpl[To, T](
      new ExprModifier:
        def apply[T <: String: Type](nameExpr: Expr[T]): Expr[Any] =
          builder.withField[Config, EnableConfig[Config, TransformerCfg.FieldComputedF[T]]](definition, nameExpr, compute)
    )(selector)

  def withFieldRenamedImpl[Definition[_ <: Tuple], From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[Definition[Config]],
    builder: FinalDefinitionBuilder[Definition]
  )(
    renameFrom: Expr[From => T],
    renameTo: Expr[To => T]
  )(using Quotes): Expr[Any] =
    withFieldGenImpl[From, To, T](
      new ExprModifierRename:
        def apply[F <: String: Type, T <: String: Type](renameFrom: Expr[F], renameTo: Expr[T]): Expr[Any] =
          builder.withNewConfig[Config, EnableConfig[Config, TransformerCfg.FieldRelabelled[F, T]]](definition)
    )(renameFrom, renameTo)

  private def withFieldGenImpl[To: Type, T: Type](
    concrete: ExprModifier
  )(
    selector: Expr[To => T]
  )(using quotes: Quotes): Expr[Any] = 
    import quotes.reflect.report
    val nameExpr = MacroUtils.extractNameFromSelectorImpl(selector)
    nameExpr match
      case '{$name: t} =>
          concrete(name)
      case _ =>
          report.throwError("Unable to extract selector name")
  end withFieldGenImpl

  private def withFieldGenImpl[From: Type, To: Type, T: Type](
    concrete: ExprModifierRename
  )(
    renameFrom: Expr[From => T],
    renameTo: Expr[To => T]
  )(using quotes: Quotes): Expr[Any] = 
    import quotes.reflect.report
    val renameFromExpr = MacroUtils.extractNameFromSelectorImpl(renameFrom)
    val renameToExpr = MacroUtils.extractNameFromSelectorImpl(renameTo)
    (renameFromExpr, renameToExpr) match
      case ('{$renameFrom: f}, '{$renameTo: t}) =>
          concrete(renameFrom, renameTo)
      case _ =>
          report.throwError("Unable to extract selectors names")
  end withFieldGenImpl

  trait ExprModifier {
    def apply[T <: String: Type](nameExpr: Expr[T]): Expr[Any]
  }

  trait ExprModifierRename {
    def apply[F <: String: Type, T <: String: Type](renameFrom: Expr[F], renameTo: Expr[T]): Expr[Any]
  }

  trait FinalDefinitionBuilder[Definition[_ <: Tuple]] {
    def withField[OldConfig <: Tuple: Type, NewConfig <: Tuple: Type](old: Expr[Definition[OldConfig]], name: Expr[String], value: Expr[Any]): Expr[Definition[NewConfig]]
    def withNewConfig[OldConfig <: Tuple: Type, NewConfig <: Tuple: Type](old: Expr[Definition[OldConfig]]): Expr[Definition[NewConfig]]
  }

  class TransformerDefinitionBuilder[From: Type, To: Type, Flags <: Tuple: Type](using Quotes) extends 
    FinalDefinitionBuilder[[Config <: Tuple] =>> TransformerDefinition[From, To, Config, Flags]]:
    def withField[OldConfig <: Tuple: Type, NewConfig <: Tuple: Type](old: Expr[TransformerDefinition[From, To, OldConfig, Flags]], name: Expr[String], value: Expr[Any]): Expr[TransformerDefinition[From, To, NewConfig, Flags]] = '{
      TransformerDefinition(
        overrides = $old.overrides + ($name -> $value),
        instances = $old.instances
      )
    }

    def withNewConfig[OldConfig <: Tuple: Type, NewConfig <: Tuple: Type](old: Expr[TransformerDefinition[From, To, OldConfig, Flags]]): Expr[TransformerDefinition[From, To, NewConfig, Flags]] = '{
      $old.asInstanceOf[TransformerDefinition[From, To, NewConfig, Flags]]
    }
  
  class TransformerFDefinitionBuilder[F[_]: Type, From: Type, To: Type, Flags <: Tuple: Type](using Quotes) extends 
    FinalDefinitionBuilder[[Config <: Tuple] =>> TransformerFDefinition[F, From, To, Config, Flags]]:
    def withField[OldConfig <: Tuple: Type, NewConfig <: Tuple: Type](old: Expr[TransformerFDefinition[F, From, To, OldConfig, Flags]], name: Expr[String], value: Expr[Any]): Expr[TransformerFDefinition[F, From, To, NewConfig, Flags]] = '{
      TransformerFDefinition(
        overrides = $old.overrides + ($name -> $value),
        instances = $old.instances
      )
    }

    def withNewConfig[OldConfig <: Tuple: Type, NewConfig <: Tuple: Type](old: Expr[TransformerFDefinition[F, From, To, OldConfig, Flags]]): Expr[TransformerFDefinition[F, From, To, NewConfig, Flags]] = '{
      $old.asInstanceOf[TransformerFDefinition[F, From, To, NewConfig, Flags]]
    }
  
end GenericTransformerDefinitionBuilder
