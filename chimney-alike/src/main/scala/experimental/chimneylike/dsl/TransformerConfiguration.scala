package experimental.chimneylike.dsl

import experimental.chimneylike.internal.TransformerFlags.*
import experimental.chimneylike.internal.*

class TransformerConfiguration[Flags <: TransformerFlags]
    extends FlagsDsl[[F1 <: TransformerFlags] =>> TransformerConfiguration[F1], Flags]

object TransformerConfiguration:
  given default: TransformerConfiguration[TransformerFlags.Default] = new TransformerConfiguration[TransformerFlags.Default]
end TransformerConfiguration