package experimental.chimneylike.dsl

import experimental.chimneylike.internal.TransformerFlag.*
import experimental.chimneylike.internal.dsl.*
import experimental.chimneylike.internal.*
import experimental.chimneylike.Transformer
import experimental.chimneylike.internal.utils.MacroUtils

/** Allows customization of [[io.scalaland.chimney.TransformerF]] derivation
  *
  * @tparam F    wrapper type constructor
  * @tparam From type of input value
  * @tparam To   type of output value
  * @tparam C    type-level encoded config
  */
final class TransformerFDefinition[F[_], From, To, Config <: Tuple, Flags <: Tuple](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends FlagsDsl[[FS <: Tuple] =>> TransformerFDefinition[F, From, To, Config, FS], Flags]:

/** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldConst[T, U](inline selector: To => T, value: U) = 
    TransformerFDefinitionBuilder.withFieldConst(this)(selector, value)

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldConstF[T](inline selector: To => T, value: F[T]) = 
    TransformerFDefinitionBuilder.withFieldConstF(this)(selector, value)

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[experimental.chimneylike.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldComputed[T](inline selector: To => T, map: From => T) = 
    TransformerFDefinitionBuilder.withFieldComputed(this)(selector, map)

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[experimental.chimneylike.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldComputedF[T](inline selector: To => T, map: From => F[T]) = 
    TransformerFDefinitionBuilder.withFieldComputedF(this)(selector, map)

end TransformerFDefinition