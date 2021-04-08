package experimental.chimneylike.internal.utils

class ArrayProduct[T](underlying: Array[T]) extends Product {
  override def productArity: Int = underlying.length

  override def productElement(n: Int): Any = underlying(n)

  override def canEqual(that: Any): Boolean = that.isInstanceOf[ArrayProduct[_]]
}
