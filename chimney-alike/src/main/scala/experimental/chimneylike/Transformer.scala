package experimental.chimneylike

import internal.derived.TransformerDerive
import internal.TransformerFlag
import dsl.TransformerDefinition
import scala.compiletime.error

trait Transformer[From, To]:
  def transform(from: From): To
  extension(from: From)
    inline def transformTo: To = transform(from)
end Transformer

object Transformer:

  inline def derived[From, To]: Transformer[From, To] = 
    TransformerDerive.derived[From, To, EmptyTuple, TransformerFlag.DefaultValues *: EmptyTuple](
      TransformerDefinition(Map.empty, Map.empty)
    )
end Transformer