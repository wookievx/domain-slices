package experimental.chimneylike.dsl

import experimental.chimneylike.internal.TransformerFlags._
import experimental.chimneylike.internal._

final class TransformerInto[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
  val source: From,
  val definition: TransformerDefinition[From, To, C, Flags]
)