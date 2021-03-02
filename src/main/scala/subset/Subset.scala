package subset
import scala.deriving.*
import scala.compiletime.*
import scala.compiletime.ops.int._
import scala.quoted.*
import scala.collection.mutable.Builder

trait SubsetOf[S, T] with
  def extractFrom(from: T): S
  def applyOn(on: T, slice: S): T
  extension (on: T)
    inline def extract: S = extractFrom(on)
    inline def withSlice(slice: S): T = applyOn(on, slice)
  end extension
end SubsetOf


object SubsetOf with

  inline def deriveProduct[S, T](using ms: Mirror.ProductOf[S], mt: Mirror.ProductOf[T]): SubsetOf[S, T] =
    val subsetLabels = constValueTuple[ms.MirroredElemLabels]
    val targetLabels = constValueTuple[mt.MirroredElemLabels]

    val subsetPositions = subsetLabels.toList.view.view.zipWithIndex.toMap
    val targetPositions = targetLabels.toList.view.view.zipWithIndex.toMap
    ???

  
  inline def extractAll[S <: Tuple, T <: Tuple, P <: Tuple](inline positions: P): T => IArray[Any] = 
    val funs = IArray.unsafeFromArray(extractAllImpl[S, T, P, 0](positions, 0, new Array[IArray[Any] => Any](constValue[Tuple.Size[S]])))
    (t: T) => 
      val input = t.toIArray
      val res = new Array[Any](constValue[Tuple.Size[S]])
      for (fun, ind) <- funs.zipWithIndex do res(ind) = fun(input)
      IArray.unsafeFromArray(res)

  private inline def extractAllImpl[S <: Tuple, T <: Tuple, P <: Tuple, F <: Int](inline postions: P, focus: F, elems: Array[IArray[Any] => Any]): Array[IArray[Any] => Any] =
    inline erasedValue[P] match
      case _ : (h *: tail) =>
        inline constValue[h] match
          case i: Int =>
            elems(focus) = extractSingleImpl[i.type, Tuple.Elem[S, F], T](i)
            extractAllImpl[S, T, tail, F + 1](postions.drop(1).asInstanceOf[tail], (focus + 1).asInstanceOf[F + 1], elems)
          case _ =>
            error("Unexpected value in positions")
      case EmptyTuple =>
        elems 

  inline def allPositionsOf[S <: Tuple, T <: Tuple]: Map[Int, Int] =
    allPostitionOfImpl[S, T](0, Map.empty)
  
  private inline def allPostitionOfImpl[S <: Tuple, T <: Tuple](inline focus: Int, builder: Map[Int, Int]): Map[Int, Int] =
    inline erasedValue[S] match
      case _: (h *: tail) =>
        allPostitionOfImpl[tail, T](focus + 1, builder + (focus -> positionOf[h, T]))
      case EmptyTuple => builder


  inline def extractSingleImpl[P <: Int, V, T <: Tuple](inline expectedPostion: Int): IArray[Any] => Any =
    inline erasedValue[Tuple.Elem[T, P]] match
      case _: V => (array: IArray[Any]) => array(expectedPostion)
      case _ =>
        summonFrom {
          case s: SubsetOf[V, Tuple.Elem[T, P]] => (array: IArray[Any]) => 
            s.extractFrom(array(expectedPostion).asInstanceOf[Tuple.Elem[T, P]])
          case _ =>
            error(constExtractMessage[T])
        }

  transparent inline def positionOf[L, T <: Tuple]: Int = 
    positionImpl[L, T](0, constMessage[L, T])

  private inline def positionImpl[L, T <: Tuple](inline acc: Int, inline message: String): Int =
    inline erasedValue[T] match
      case _: (L *: _) => acc
      case _: (_ *: tail) => positionImpl[L, tail](acc + 1, message)
      case EmptyTuple => error(message)

  import scala.compiletime.ops.string._
  
  inline def constExtractMessage[T <: Tuple] =
    constValue["Value at a postion is not of correct type: " + TupleToString[T, ""]]    

  inline def constMessage[L, T <: Tuple] = 
    constValue["Unable to find a label in " + TupleToString[T, ""]]
  
  type TupleToString[T <: Tuple, Z <: String] <: String =
    T match
      case h *: tail =>
        TupleToString[tail, Z + ", " + h]
      case EmptyTuple => 
        Z
    

end SubsetOf