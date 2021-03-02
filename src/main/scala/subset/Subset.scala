package subset
import scala.deriving.*
import scala.compiletime.*
import scala.compiletime.ops.int._
import scala.quoted.*
import scala.collection.mutable.Builder

trait SubsetOf[S, T] with
  def extractFrom(from: T): S
  def applyOn(on: T, subset: S): T
  extension (on: T)
    inline def extract: S = extractFrom(on)
    inline def withSlice(slice: S): T = applyOn(on, slice)
  end extension
end SubsetOf


object SubsetOf with

  inline given derive[S, T](using ms: Mirror.Of[S], mt: Mirror.Of[T]): SubsetOf[S, T] =
    inline ms match
      case ms: Mirror.ProductOf[S] => 
        inline mt match
          case mt: Mirror.ProductOf[T] => deriveProduct[S, T](using ms, mt)
          case _ => error("Both types need to be product")
      case _ => 
        error("Only products are supported for now")

  inline def deriveProduct[S, T](using ms: Mirror.ProductOf[S], mt: Mirror.ProductOf[T]): SubsetOf[S, T] =
    val extractor = extractAllFinal[ms.MirroredElemLabels, ms.MirroredElemTypes, mt.MirroredElemLabels, mt.MirroredElemTypes]
    new SubsetOf[S, T]:
      def extractFrom(from: T): S = ms.fromProduct(Tuple.fromIArray(extractor(Tuple.fromProduct(from.asInstanceOf[Product]).toIArray)))
      def applyOn(on: T, subset: S): T = ???

  inline def extractAllFinal[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple]: IArray[Any] => IArray[Any] =
    val functions = extractAllFinalImpl[SLabels, SValues, TLabels, TValues](List.empty)
    (array: IArray[Any]) => 
      val arr = new Array[Any](constValue[Tuple.Size[SLabels]])
      for (fun, pos) <- functions.zipWithIndex do
        arr(pos) = fun(array)
      IArray.unsafeFromArray(arr)
  
  inline def extractAllFinalImpl[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple](functions: List[IArray[Any] => Any]): List[IArray[Any] => Any] =
    inline (erasedValue[SLabels], erasedValue[SValues]) match
      case _: (label *: slabels, value *: svalues) =>
        extractAllFinalImpl[slabels, svalues, TLabels, TValues](functions :+ extractFinal[label, value, TLabels, TValues])
      case _ =>
        functions


  inline def extractFinal[Label, Value, TLables <: Tuple, TValues <: Tuple]: IArray[Any] => Any = 
    extractFinalImpl[Label, Value, TLables, TValues, 0]

  inline def extractFinalImpl[Label, Value, TLables <: Tuple, TValues <: Tuple, Pos <: Int]: IArray[Any] => Any = 
    inline erasedValue[Tuple.Elem[TLables, 0]] match
      case _: Label =>
        inline erasedValue[Tuple.Elem[TValues, 0]] match
          case _: Value =>
            (ar: IArray[Any]) => ar(constValue[Pos])
          case _ =>
            summonFrom {
              case s: (Value SubsetOf Tuple.Elem[TValues, 0]) =>
                (ar: IArray[Any]) => s.extractFrom(ar(constValue[Pos]).asInstanceOf[Tuple.Elem[TValues, 0]])
              case _ =>
                summonFrom {
                  case mv: Mirror.Of[Value] =>
                    summonFrom {
                      case mt: Mirror.Of[Tuple.Elem[TValues, 0]] =>
                        val derived = derive[Value, Tuple.Elem[TValues, 0]](using mv, mt)
                        (ar: IArray[Any]) => derived.extractFrom(ar(constValue[Pos]).asInstanceOf[Tuple.Elem[TValues, 0]])
                      case _ =>
                        import scala.compiletime.ops.string._
                        error(constValue["Illegal type of value at position (not found tuple elem): " + ToString[Pos] + " in " + TupleToString[TLables, ""]])       
                    }
                  case _ =>
                    import scala.compiletime.ops.string._
                    error(constValue["Illegal type of value at position (not found value)): " + ToString[Pos] + " in " + TupleToString[TLables, ""]])   
                }              
            }
      case _ => 
        inline (erasedValue[TLables], erasedValue[TValues]) match
          case _: (h *: tlabels, _ *: tvalues) =>
            extractFinalImpl[Label, Value, tlabels, tvalues, Pos + 1]
          case _ =>
            import scala.compiletime.ops.string._
            error(constValue["Illegal type of value in " + TupleToString[TLables, ""]])

  
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