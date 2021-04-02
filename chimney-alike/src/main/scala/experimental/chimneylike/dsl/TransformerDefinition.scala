package experimental.chimneylike.dsl

import experimental.chimneylike.internal.TransformerFlag.*
import experimental.chimneylike.internal.dsl.TransformerDefinitionBuilder
import experimental.chimneylike.internal.*
import experimental.chimneylike.Transformer
import experimental.chimneylike.internal.utils.MacroUtils

/** Allows customization of [[experimental.chimneylike.Transformer]] derivation
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  * @tparam C    type-level encoded config
  */
final class TransformerDefinition[From, To, Config <: Tuple, Flags <: Tuple](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends FlagsDsl[[FS <: Tuple] =>> TransformerDefinition[From, To, Config, FS], Flags] {

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[experimental.chimneylike.dsl.TransformerDefinition]]
    */
  transparent inline def withFieldConst[T](inline selector: To => T, value: T) = TransformerDefinitionBuilder.withFieldConst[From, To, Config, Flags, T](this)(selector, value)

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[experimental.chimneylike.dsl.TransformerDefinition]]
    */
  transparent inline def withFieldComputed[T](
      selector: To => T,
      map: From => T
  ) = ???

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[experimental.chimneylike.dsl.TransformerDefinition]]
    */
  transparent inline def withFieldRenamed[T](
      selectorFrom: From => T,
      selectorTo: To => T
  ) = ???

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[experimental.chimneylike.dsl.TransformerDefinition]]
    */
  transparent inline def withCoproductInstance[Inst](f: Inst => To) = ???

  /** Build Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[experimental.chimneylike.Transformer]] type class instance
    */
  inline def buildTransformer[ScopeFlags <: Tuple](using tc: TransformerConfiguration[ScopeFlags]): Transformer[From, To] = ???

}

def defaultDefinition[From, To]: TransformerDefinition[From, To, EmptyTuple, EmptyTuple] = TransformerDefinition(Map.empty, Map.empty)