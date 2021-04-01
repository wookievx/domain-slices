package subset
import scala.deriving.*
import scala.compiletime.*
import scala.compiletime.ops.int._
import scala.collection.mutable.Builder
import util.DebugUtils

trait SubsetOf[S, T]:
  def extractFrom(from: T): S
  def applyOn(on: T, subset: S): T
  extension (on: T)
    inline def extract: S = extractFrom(on)
    inline def withSubset(subset: S): T = applyOn(on, subset)
  end extension
end SubsetOf


object SubsetOf:
  abstract class SpecificSubsetOf[S, T](extractFn: T => S, applyFn: (T, S) => T) extends SubsetOf[S, T]:
    override def extractFrom(from: T): S = extractFn(from)
    override def applyOn(on: T, subset: S): T = applyFn(on, subset)

  inline def deriveConcrete[S, T, Concrete <: SubsetOf[S, T]](build: (T => S, (T, S) => T) => Concrete): Concrete =
    summonFrom {
      case ms: Mirror.ProductOf[S] => 
        summonFrom {
          case mt: Mirror.ProductOf[T] => deriveProductConcrete[S, T, Concrete](build)(using ms, mt)
          case _ => error("Both types need to be product")
        }
      case _ =>
        error("Only products are supported for now")
    }

  inline def deriveProductConcrete[S, T, Concrete <: SubsetOf[S, T]](build: (T => S, (T, S) => T) => Concrete)(using ms: Mirror.ProductOf[S], mt: Mirror.ProductOf[T]): Concrete =
    DebugUtils.debug {
      build(
        (from: T) =>
          val inputArray = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
          val outputArray = extractAllFinal[ms.MirroredElemLabels, ms.MirroredElemTypes, mt.MirroredElemLabels, mt.MirroredElemTypes](inputArray)
          ms.fromProduct(Tuple.fromIArray(outputArray)),
        (on: T, subset: S) => 
          val onInputArray = Tuple.fromProduct(on.asInstanceOf[Product]).toIArray
          val subsetArray = Tuple.fromProduct(subset.asInstanceOf[Product]).toIArray
          val outputArray = applyAll[ms.MirroredElemLabels, ms.MirroredElemTypes, mt.MirroredElemLabels, mt.MirroredElemTypes](onInputArray, subsetArray)
          mt.fromProduct(Tuple.fromIArray(outputArray))
      )
    }
    

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
  inline def deriveProduct[S, T](using ms: Mirror.ProductOf[S], mt: Mirror.ProductOf[T]): SubsetOf[S, T] = DebugUtils.debug {
    new SubsetOf[S, T]:
      def extractFrom(from: T): S = 
        val inputArray = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
        val outputArray = extractAllFinal[ms.MirroredElemLabels, ms.MirroredElemTypes, mt.MirroredElemLabels, mt.MirroredElemTypes](inputArray)
        ms.fromProduct(Tuple.fromIArray(outputArray))
      def applyOn(on: T, subset: S): T =
        val onInputArray = Tuple.fromProduct(on.asInstanceOf[Product]).toIArray
        val subsetArray = Tuple.fromProduct(subset.asInstanceOf[Product]).toIArray
        val outputArray = applyAll[ms.MirroredElemLabels, ms.MirroredElemTypes, mt.MirroredElemLabels, mt.MirroredElemTypes](onInputArray, subsetArray)
        mt.fromProduct(Tuple.fromIArray(outputArray))
  }    

  inline def extractAllFinal[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple](source: IArray[Any]): IArray[Any] =
    val arr = new Array[Any](constValue[Tuple.Size[SLabels]])
    extractAllFinalImpl[SLabels, SValues, TLabels, TValues, 0](source, arr)
    IArray.unsafeFromArray(arr)
  
  inline def extractAllFinalImpl[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple, Pos <: Int](source: IArray[Any], target: Array[Any]): Unit =
    inline (erasedValue[SLabels], erasedValue[SValues]) match
      case _: (label *: slabels, value *: svalues) =>
        target(constValue[Pos]) = extractFinal[label, value, TLabels, TValues](source)
        extractAllFinalImpl[slabels, svalues, TLabels, TValues, Pos + 1](source, target)
      case _ =>


  inline def extractFinal[Label, Value, TLables <: Tuple, TValues <: Tuple](source: IArray[Any]): Any = 
    extractFinalImpl[Label, Value, TLables, TValues, 0](source)

  inline def extractFinalImpl[Label, Value, TLables <: Tuple, TValues <: Tuple, Pos <: Int](source: IArray[Any]): Any = 
    inline erasedValue[Tuple.Elem[TLables, 0]] match
      case _: Label =>
        inline erasedValue[Tuple.Elem[TValues, 0]] match
          case _: Value =>
            source(constValue[Pos])
          case _ =>
            summonFrom {
              case s: (Value SubsetOf Tuple.Elem[TValues, 0]) =>
                s.extractFrom(source(constValue[Pos]).asInstanceOf)
              case _ =>
                import scala.compiletime.ops.string._
                error(constValue["Illegal type of value in " + TupleToString[TLables, ""]])              
            }
      case _ => 
        inline (erasedValue[TLables], erasedValue[TValues]) match
          case _: (h *: tlabels, _ *: tvalues) =>
            extractFinalImpl[Label, Value, tlabels, tvalues, Pos + 1](source)
          case _ =>
            import scala.compiletime.ops.string._
            error(constValue["Illegal type of value in " + TupleToString[TLables, ""]])

  //applying subset implementation
  inline def applyAll[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple](on: IArray[Any], subset: IArray[Any]): IArray[Any] =
    val outArray: Array[Any] = new Array[Any](on.length)
    on.copyToArray(outArray)
    applyAllImpl[SLabels, SValues, TLabels, TValues, 0](outArray, subset)
    IArray.unsafeFromArray(outArray)


  inline def applyAllImpl[SLabels <: Tuple, SValues <: Tuple, TLabels <: Tuple, TValues <: Tuple, SPos <: Int](onTarget: Array[Any], subset: IArray[Any]): Unit =
    inline (erasedValue[SLabels], erasedValue[SValues]) match
      case _: (label *: slabels, value *: svalues) => 
        applyImpl[label, value, TLabels, TValues, 0](onTarget, subset(constValue[SPos]))
        applyAllImpl[slabels, svalues, TLabels, TValues, SPos + 1](onTarget, subset)
      case _ => 

  inline def applyImpl[Label, Value, TLabels <: Tuple, TValues <: Tuple, Pos <: Int](onTarget: Array[Any], value: Any): Unit =
    inline erasedValue[Tuple.Elem[TLabels, 0]] match
      case _: Label =>
        inline erasedValue[Tuple.Elem[TValues, 0]] match
          case _: Value =>
            onTarget(constValue[Pos]) = value
          case _ =>
            summonFrom {
              case s: (Value SubsetOf Tuple.Elem[TValues, 0]) =>
                onTarget(constValue[Pos]) = s.applyOn(onTarget(constValue[Pos]).asInstanceOf, value.asInstanceOf)
              case _ =>
                import scala.compiletime.ops.string._
                error(constValue["Illegal type of value at position: " + ToString[Pos] + " in " + TupleToString[TLabels, ""]])
            }
      case _ =>
        inline (erasedValue[TLabels], erasedValue[TValues]) match
          case _: (h *: tlabels, _ *: tvalues) =>
            applyImpl[Label, Value, tlabels, tvalues, Pos + 1](onTarget, value)
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