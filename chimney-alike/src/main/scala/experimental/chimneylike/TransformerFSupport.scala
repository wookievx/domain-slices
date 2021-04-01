package experimental.chimneylike

import scala.collection.Factory

trait TransformerFSupport[F[+_]]:

  /** Wrap a value into the type constructor `F`.
    *
    * @param value value to wrap
    * @tparam A type of value
    * @return wrapped value
    */
  def pure[A](value: A): F[A]

  /** Combine two wrapped values into wrapped pair of them.
    *
    * This method allows to decide on error handling semantics for
    * given type `F`.
    *
    * @param fa first wrapped value
    * @param fb second wrapped value
    * @tparam A type of first value
    * @tparam B type of second value
    * @return wrapped pair of values
    */
  def product[A, B](fa: F[A], fb: => F[B]): F[(A, B)]

  /** Transform wrapped value with given function.
    *
    * @param fa wrapped value
    * @param f function
    * @tparam A type of wrapped value
    * @tparam B result type of provided function `f`
    * @return wrapped result of function `f` applied to un
    */
  def map[A, B](fa: F[A], f: A => B): F[B]

  /** Perform traversal of function `f` on provided iterator of elements.
    *
    * Primarily used to perform recursive lifted transformation (given as function `f`) on a collection
    * type (Array, Seq, List, Vector, Map, etc.) for which we can obtain an `Iterator[A]`.
    *
    * This method allows to decide on error handling semantics for given type `F`, when transforming
    * between collections.
    *
    * @param it  iterator of elements of type `A`
    * @param f   function to apply to elements of type `A`, returning `F[B]`
    * @param fac factory for collection type `M`
    * @tparam M  type of collection where transformed elements are stored; note that this is not
    *            a type constructor, but a type with applied argument, so it can be List[B], Map[K, V], etc.
    * @tparam A  type of elements being iterated
    * @tparam B  target element type of function `f`
    * @return wrapped collection of type `F[M]`
    */
  def traverse[M, A, B](it: Iterator[A], f: A => F[B])(using fac: Factory[B, M]): F[M]

end TransformerFSupport