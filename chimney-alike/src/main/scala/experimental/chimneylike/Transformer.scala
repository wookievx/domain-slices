package experimental.chimneylike

trait Transformer[From, To]:
  def transform(from: From): To
  extension(from: From)
    inline def transformTo: To = transform(from)
end Transformer