package experimental.chimneylike.dsl

import experimental.chimneylike.internal.TransformerFlag._
import experimental.chimneylike.internal._

final class TransformerInto[From, To, Config <: Tuple, Flags <: Tuple](
  val source: From,
  val definition: TransformerDefinition[From, To, Config, Flags]
)