package experimental.chimneylike

case class TransformationError[M](message: M, errorPath: List[ErrorPathNode] = Nil):
  def prepend(node: ErrorPathNode): TransformationError[M] =
    TransformationError[M](message, node :: errorPath)

  def showPathError: String =
    errorPath match
      case head :: tail =>
        tail.foldLeft(head.show)((acc, next) => acc + next.separator + next.show)
      case Nil => ""

end TransformationError