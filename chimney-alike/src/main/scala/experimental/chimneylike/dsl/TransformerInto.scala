package experimental.chimneylike.dsl

import experimental.chimneylike.internal.TransformerFlag._
import experimental.chimneylike.internal._
import scala.compiletime.error

final class TransformerInto[From, To, Config <: Tuple, Flags <: Tuple](
  val source: From,
  val definition: TransformerDefinition[From, To, Config, Flags]
) extends FlagsDsl[[FS <: Tuple] =>> TransformerInto[From, To, Config, FS], Flags]:

  /** Lifts current transformation with provided type constructor `F`.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @tparam F    wrapper type constructor
    * @return [[experimental.chimneylike.dsl.TransformerFInto]]
    */
  inline def lift[F[_]]: TransformerFInto[F, From, To, EnableConfig[Config, TransformerCfg.WrapperType[F]], Flags] =
    TransformerFInto(source, definition.lift[F])

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @return [[experimental.chimneylike.dsl.TransformerInto]]
    */
  transparent inline def withFieldConst[T](inline selector: To => T, value: T) = 
    withDefinition(definition.withFieldConst(selector, value))

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[experimental.chimneylike.dsl.TransformerInto]]
    */
  transparent inline def withFieldConstF[F[_], T](inline selector: To => T, value: F[T]) = 
    withDefinitionF[F](definition.withFieldConstF(selector, value))

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[experimental.chimneylike.dsl.TransformerInto]]
    */
  transparent inline def withFieldComputed[T](inline selector: To => T, map: From => T) = 
    withDefinition(definition.withFieldComputed(selector, map))
  
  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[experimental.chimneylike.dsl.TransformerInto]]
    */
  transparent inline def withFieldComputedF[F[_], T](inline selector: To => T, map: From => F[T]) = 
    withDefinitionF[F](definition.withFieldComputedF(selector, map))

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[experimental.chimneylike.dsl.TransformerInto]]
    */
  transparent inline def withFieldRenamed[T](inline selectorFrom: From => T, inline selectorTo: To => T) = 
    withDefinition(definition.withFieldRenamed(selectorFrom, selectorTo))

  transparent inline def withDefinition(inline newDefinition: Any) =
    inline newDefinition match
      case definition: TransformerDefinition[From, To, config, flags] =>
        TransformerInto(source, definition)
      case _ =>
        error("Changing definition failed, should not happen, a bug in library")

  transparent inline def withDefinitionF[F[_]](inline newDefinition: Any) =
    inline newDefinition match
      case definition: TransformerFDefinition[F, From, To, config, flags] =>
        TransformerFInto(source, definition)
      case _ =>
        error("Changing definition failed, should not happen, a bug in library")

end TransformerInto

extension[From](source: From)
  inline def into[To]: TransformerInto[From, To, EmptyTuple, EmptyTuple] =
    TransformerInto(source, defaultDefinition[From, To])
