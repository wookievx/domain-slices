package experimental.chimneylike

trait TransformerF[F[_], From, To]:
  def transform(from: From): F[To]
  extension(from: From)
    inline def transformTo: F[To] = transform(from)
end TransformerF