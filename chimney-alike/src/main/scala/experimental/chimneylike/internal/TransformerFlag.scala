package experimental.chimneylike.internal

import scala.compiletime.ops.any.{!=, ==}

sealed abstract class TransformerFlag
object TransformerFlag:
  final class MethodAccessors extends TransformerFlag
  final class DefaultValues extends TransformerFlag
  final class BeanSetters extends TransformerFlag
  final class BeanGetters extends TransformerFlag
  final class OptionDefaultsToNone extends TransformerFlag
  final class UnsafeOption extends TransformerFlag
end TransformerFlag

import TransformerFlag._

type EnableFlag[Flags <: Tuple, Flag <: TransformerFlag] = Flag *: DisableFlag[Flags, Flag]

type DisableFlag[Flags <: Tuple, Flag <: TransformerFlag] <: Tuple = (Flags, Flag) match
  case (MethodAccessors *: tail, MethodAccessors) => tail
  case (DefaultValues *: tail, DefaultValues) => tail
  case (BeanSetters *: tail, BeanSetters) => tail
  case (BeanGetters *: tail, BeanGetters) => tail
  case (OptionDefaultsToNone *: tail, OptionDefaultsToNone) => tail
  case (UnsafeOption *: tail, UnsafeOption) => tail
  case (h *: tail, _) => h *: DisableFlag[tail, Flag]
  case (EmptyTuple, _) => EmptyTuple
