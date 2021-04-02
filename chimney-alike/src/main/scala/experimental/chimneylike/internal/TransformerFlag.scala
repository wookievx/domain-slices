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

type EnableFlag[Flags <: Tuple, Flag <: TransformerFlag] = Flag *: DisableFlag[Flags, Flag]

type DisableFlag[Flags <: Tuple, Flag <: TransformerFlag] = Tuple.Filter[Flags, IsDifferentFlag[Flag]]

type IsDifferentFlag[F] = [Flag] =>> F != Flag