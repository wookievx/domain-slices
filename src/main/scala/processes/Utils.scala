package processes

import scala.compiletime.ops.any._
import scala.quoted._

object Utils:

  type ContainedIn[A <: Tuple, B <: Tuple] <: Boolean = (A, B) match
    case (a *: ta, b *: tb) =>
      a == b match
        case true => ContainedIn[ta, tb]
        case false => ContainedIn[ta, B]
    case (_, EmptyTuple) => true
    case (EmptyTuple, _) => false

  
  extension [T](inline arr: Array[T])
    inline def fastForeach(inline f: T => Unit): Unit = ${superFastForeach('arr, 'f)}
    
  
  def superFastForeach[A: Type](arr: Expr[Array[A]], f: Expr[A => Unit])(using Quotes): Expr[Unit] =
    '{
      var ind = 0
      while ind < ${arr}.length do
        ${Expr.betaReduce('{${f}(${arr}(ind))})}
        ind += 1
    }
    


end Utils