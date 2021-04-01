package experimental.chimneylike

trait TransformerFErrorPathSupport[F[+_]]:

  /** Prepend node of path to each error in wrapped value.
    *
    * @param fa wrapped value
    * @param node previous node of path
    * @tparam A type of value
    * @return wrapped value with added node in errors
    */
  def addPath[A](fa: F[A], node: ErrorPathNode): F[A]

end TransformerFErrorPathSupport