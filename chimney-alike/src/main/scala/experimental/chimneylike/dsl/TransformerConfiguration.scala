package experimental.chimneylike.dsl

import experimental.chimneylike.internal.TransformerFlag._
import experimental.chimneylike.internal._

class TransformerConfiguration[Flags <: Tuple]
    extends FlagsDsl[[F1 <: Tuple] =>> TransformerConfiguration[F1], Flags]

object TransformerConfiguration:
  given default: TransformerConfiguration[EmptyTuple] = new TransformerConfiguration[EmptyTuple]
end TransformerConfiguration