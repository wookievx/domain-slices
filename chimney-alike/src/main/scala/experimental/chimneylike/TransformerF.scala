package experimental.chimneylike

import internal.derived.TransformerDerive
import internal.TransformerFlag
import dsl.TransformerFDefinition
import scala.compiletime.error

trait TransformerF[F[_], From, To]:
  def transform(from: From): F[To]
  extension(from: From)
    inline def transformTo: F[To] = transform(from)
end TransformerF

object TransformerF:
  inline def derived[F[_], From, To](using sup: TransformerFSupport[F]): TransformerF[F, From, To] = 
    TransformerDerive.derived[F, From, To, EmptyTuple, TransformerFlag.DefaultValues *: EmptyTuple](
      TransformerFDefinition(Map.empty, Map.empty)
    )
end TransformerF

