package experimental.chimneylike.internal.dsl

import experimental.chimneylike.dsl.TransformerDefinition
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*

import scala.quoted.{given, *}
import deriving.*, compiletime.*

object TransformerDefinitionBuilder:

  transparent inline def withFieldConst[From, To, Config <: Tuple, Flags <: Tuple, T](
    definition: TransformerDefinition[From, To, Config, Flags])(inline selector: To => T, value: T) = ${withFieldConstImpl[From, To, Config, Flags, T]('definition)('selector, 'value)}

  private def withFieldConstImpl[From: Type, To: Type, Config <: Tuple: Type, Flags <: Tuple: Type, T: Type](
    definition: Expr[TransformerDefinition[From, To, Config, Flags]])(selector: Expr[To => T], value: Expr[T])(using quotes: Quotes): Expr[Any] = 
    import quotes.reflect.report
    val name = MacroUtils.extractNameFromSelectorImpl(selector)
    name match {
      case '{$s: t} =>
        Expr.summon[ConstFieldModifier[t]] match
          case Some(inst) =>
            '{$inst.addField[From, To, Config, Flags, T]($definition, $name, $value)}
          case None =>
            report.throwError(s"Illegal name returned: ${name.show}, probably a bug")
      case t =>
        report.throwError(s"Illegal name returned: ${t.show}, probably a bug")
    }
  end withFieldConstImpl

  trait ConstFieldModifier[N]:
    type Name <: String
    inline def addField[From, To, Config <: Tuple, Flags <: Tuple, T](
      definition: TransformerDefinition[From, To, Config, Flags],
      name: String,
      value: T
    ): TransformerDefinition[From, To, EnableConfig[Config, TransformerCfg.FieldConst[Name]], Flags] = 
        TransformerDefinition(overrides = definition.overrides + (name -> value), instances = definition.instances)


  inline given default[N <: String]: ConstFieldModifier[N] with
    type Name = N

  
end TransformerDefinitionBuilder