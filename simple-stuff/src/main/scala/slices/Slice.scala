package slices
import scala.deriving._
import scala.compiletime._
import scala.compiletime.ops.string._
import scala.quoted._

object Slice {

  inline given derived[T <: Product, S <: Product](using mt: Mirror.ProductOf[T], ms: Mirror.ProductOf[S]): Slice[T, S] = 
    val _extract = extractImpl[mt.MirroredElemTypes, ms.MirroredElemTypes, mt.MirroredElemLabels, ms.MirroredElemLabels]
    val _applyTo = applyImpl[mt.MirroredElemTypes, ms.MirroredElemTypes, mt.MirroredElemLabels, ms.MirroredElemLabels]
    new Slice[T, S]:
      def extractFrom(from: T): S = ms.fromProduct(_extract(Tuple.fromProductTyped[T](from)))
      def applyOn(on: T, slice: S): T = mt.fromProduct(_applyTo(Tuple.fromProductTyped[T](on), Tuple.fromProductTyped[S](slice)))


  type HeadOf[T] = T match
    case (x, _) => x
    case _ => T

  type TailOf[T] = T match
    case (_, x) => x
    case _ => T

  type Heads[T <: Tuple] = Tuple.Map[T, HeadOf]
  type Tails[T <: Tuple] = Tuple.Map[T, TailOf]


  private inline def extractImpl[T <: Tuple, S <: Tuple, LT <: Tuple, LS <: Tuple]: T => S = 
    inline (erasedValue[Tuple.Zip[LT, T]], erasedValue[Tuple.Zip[LS, S]]) match
      case _: ((labelT, t) *: ts, (labelS, s) *: ss) =>
        inline if constValue[labelT] == constValue[labelS] then
          inline erasedValue[s] match 
            case _: t =>
              val tail = extractImpl[Tails[ts], Tails[ss], Heads[ts], Heads[ss]]
              ((arg: t *: Tails[ts]) => arg.head *: tail(arg.tail)).asInstanceOf[T => S]
            case _ =>
              summonFrom {
                case s: Slice[t, s] =>
                  val tail = extractImpl[Tails[ts], Tails[ss], Heads[ts], Heads[ss]]
                  ((arg: t *: Tails[ts]) => (s.extractFrom(arg.head) *: tail(arg.tail))).asInstanceOf[T => S]
                case _ =>
                  error("Attempting to extract slice when field types do not match")
              }
        else
          val tail = extractImpl[Tails[ts], S, Heads[ts], LS]
          ((arg: t *: Tails[ts]) => tail(arg.tail)).asInstanceOf[T => S]
      case _: (t *: ts, EmptyTuple | Tuple.Zip[Heads[EmptyTuple], Tails[EmptyTuple]]) => (_: T) => ().asInstanceOf[S]
      case _: (a, b) => 
        error("Did not find all of the expected elements")

  private inline def applyImpl[T <: Tuple, S <: Tuple, LT <: Tuple, LS <: Tuple]: (T, S) => T =
    inline (erasedValue[Tuple.Zip[LT, T]], erasedValue[Tuple.Zip[LS, S]]) match
      case _: ((labelT, t) *: ts, (labelS, s) *: ss) =>
        inline if constValue[labelT] == constValue[labelS] then
          inline erasedValue[s] match
            case _: t =>
              
              val tail = applyImpl[Tails[ts], Tails[ss], Heads[ts], Heads[ss]]
              ((on: t *: Tails[ts], slice: s *: Tails[ss]) => slice.head *: tail(on.tail, slice.tail)).asInstanceOf[(T, S) => T]
            case _ =>
              summonFrom {
                case s: Slice[t, s] =>
                  val tail = applyImpl[Tails[ts], Tails[ss], Heads[ts], Heads[ss]]
                  ((on: t *: Tails[ts], slice: s *: Tails[ss]) => s.applyOn(on.head, slice.head) *: tail(on.tail, slice.tail)).asInstanceOf[(T, S) => T]
                case _ =>
                  error("Attempting to apply slice when field types do not match")
              }
        else 
          val tail = applyImpl[Tails[ts], S, Heads[ts], LS]
          ((on: t *: Tails[ts], slice: S) => on.head *: tail(on.tail, slice)).asInstanceOf[(T, S) => T]
      case _: (t *: ts, EmptyTuple | Tuple.Zip[Heads[EmptyTuple], Tails[EmptyTuple]]) =>
        (on: T, _: S) => on
      case _: (a, b) => 
        error("Did not find all of the expected elements")

  private inline def summonAll[T <: Tuple]: List[ShowType[_]] =
   inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (Tuple.Zip[Heads[EmptyTuple], Tails[EmptyTuple]]) => Nil
      case _: (t *: ts) => summonInline[ShowType[t]] :: summonAll[ts]
  
  private inline def showTuple[T <: Tuple] = 
    val summoned = summonAll[T]
    summoned.map(_.show).mkString("(", ", ", ")")

  trait ShowType[X] {
    def show: String
  }

  given ShowType[Int] with
    def show: String = "Int"

  given stringShow[T <: String]: ShowType[T] with
    def show: String = "String"

  given pairShow[A, B](using showA: ShowType[A], showB: ShowType[B]): ShowType[(A, B)] with
    def show: String = s"(${showA.show}, ${showB.show})"

  given showMeEmpty: ShowType[EmptyTuple] with
    def show: String = "()"

}


trait Slice[T, S]:
  def extractFrom(from: T): S
  def applyOn(on: T, slice: S): T
  extension (on: T)
    inline def extract: S = extractFrom(on)
    inline def withSlice(slice: S): T = applyOn(on, slice)