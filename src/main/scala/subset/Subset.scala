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
    inline def withSubset(subset: S): T = applyOn(on, subset)
  end extension
end SubsetOf


object SubsetOf with

  inline given default[S, T]: SubsetOf[S, T] = derive[S, T]

  inline def derive[S, T]: SubsetOf[S, T] =
    summonFrom {
      case ms: Mirror.ProductOf[S] => 
        summonFrom {
          case mt: Mirror.ProductOf[T] => deriveProduct[S, T](using ms, mt)
          case _ => error("Both types need to be product")
        }
      case _ =>
        error("Only products are supported for now")
    }

  //extracting subset implementation  
  inline def deriveProduct[S, T](using ms: Mirror.ProductOf[S], mt: Mirror.ProductOf[T]): SubsetOf[S, T] =
    val extractor = extractAllFinal[ms.MirroredElemLabels, ms.MirroredElemTypes, mt.MirroredElemLabels, mt.MirroredElemTypes]
    val applier = applyAll[ms.MirroredElemLabels, ms.MirroredElemTypes, mt.MirroredElemLabels, mt.MirroredElemTypes]
    new SubsetOf[S, T]:
      def extractFrom(from: T): S = 
        val inputArray = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
        ms.fromProduct(Tuple.fromIArray(extractor(inputArray)))
      def applyOn(on: T, subset: S): T =
        val onInputArray = Tuple.fromProduct(on.asInstanceOf[Product]).toIArray
        val subsetArray = Tuple.fromProduct(subset.asInstanceOf[Product]).toIArray
        mt.fromProduct(Tuple.fromIArray(applier(onInputArray, subsetArray)))

  inline def extractAllFinal[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple]: IArray[Any] => IArray[Any] =
    val functions = extractAllFinalImpl[SLabels, SValues, TLabels, TValues](List.empty)
    (array: IArray[Any]) => 
      val arr = new Array[Any](constValue[Tuple.Size[SLabels]])
      for (fun, pos) <- functions.reverseIterator.zipWithIndex do
        arr(pos) = fun(array)
      IArray.unsafeFromArray(arr)
  
  inline def extractAllFinalImpl[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple](functions: List[IArray[Any] => Any]): List[IArray[Any] => Any] =
    inline (erasedValue[SLabels], erasedValue[SValues]) match
      case _: (label *: slabels, value *: svalues) =>
        extractAllFinalImpl[slabels, svalues, TLabels, TValues](extractFinal[label, value, TLabels, TValues] :: functions)
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
                import scala.compiletime.ops.string._
                error(constValue["Illegal type of value in " + TupleToString[TLables, ""]])              
            }
      case _ => 
        inline (erasedValue[TLables], erasedValue[TValues]) match
          case _: (h *: tlabels, _ *: tvalues) =>
            extractFinalImpl[Label, Value, tlabels, tvalues, Pos + 1]
          case _ =>
            import scala.compiletime.ops.string._
            error(constValue["Illegal type of value in " + TupleToString[TLables, ""]])

  //applying subset implementation
  inline def applyAll[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple]: (IArray[Any], IArray[Any]) => IArray[Any] =
    val functions = applyAllImpl[SLabels, SValues, TLabels, TValues, 0](List.empty)
    (on: IArray[Any], subset: IArray[Any]) =>
      val outArray: Array[Any] = new Array[Any](on.length)
      on.copyToArray(outArray)
      for fun <- functions do fun(outArray, subset)
      IArray.unsafeFromArray(outArray)


  inline def applyAllImpl[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple, SPos <: Int](functions: List[(Array[Any], IArray[Any]) => Unit]): List[(Array[Any], IArray[Any]) => Unit] =
    inline (erasedValue[SLabels], erasedValue[SValues]) match
      case _: (label *: slabels, value *: svalues) => 
        val fun = (on: Array[Any], subset: IArray[Any]) => applyImpl[label, value, TLabels, TValues, 0](on, subset(constValue[SPos]))
        applyAllImpl[slabels, svalues, TLabels, TValues, SPos + 1](fun :: functions)
      case _ => functions

  inline def applyImpl[Label, Value, TLabels <: Tuple, TValues <: Tuple, Pos <: Int]: (Array[Any], Any) => Unit =
    inline erasedValue[Tuple.Elem[TLabels, 0]] match
      case _: Label =>
        inline erasedValue[Tuple.Elem[TValues, 0]] match
          case _: Value =>
            (ar: Array[Any], mod: Any) => ar(constValue[Pos]) = mod
          case _ =>
            summonFrom {
              case s: (Value SubsetOf Tuple.Elem[TValues, 0]) =>
                (ar: Array[Any], mod: Any) => ar(constValue[Pos]) = s.applyOn(ar(constValue[Pos]).asInstanceOf, mod.asInstanceOf)
              case _ =>
                import scala.compiletime.ops.string._
                error(constValue["Illegal type of value at position: " + ToString[Pos] + " in " + TupleToString[TLabels, ""]])
            }
      case _ =>
        inline (erasedValue[TLabels], erasedValue[TValues]) match
          case _: (h *: tlabels, _ *: tvalues) =>
            applyImpl[Label, Value, tlabels, tvalues, Pos + 1]
          case _ =>
            import scala.compiletime.ops.string._
            error(constValue["Not found in target type a key"])

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