package subset
import scala.deriving.*
import scala.compiletime.*

trait MyTypeclass[A, B] with
  extension (a: A)
     def doStuff(b: B): Unit
end MyTypeclass

object MyTypeclass with
  inline given derived[A, B](using am: Mirror.ProductOf[A]): MyTypeclass[A, B] =
    val bug = impl[am.MirroredElemTypes, 0, B]
    new MyTypeclass[A, B]:
      extension (a: A)
        def doStuff(b: B): Unit = bug

  //what this implementation checks, is whether element Pos of tuple A has type Expecting or there is some typeclass (which potentially might be derived automatically)
  inline def impl[A <: Tuple, Pos <: Int, Expecting]: Unit =
    inline erasedValue[Tuple.Elem[A, Pos]] match
      case _: Expecting => 
      case _ =>
        summonFrom {
          case proof: MyTypeclass[Tuple.Elem[A, Pos], Expecting] => //cannot work with "derived", but when derived instance is defined explicitly in the scope it works
          case _ =>
            summonFrom {
              case ma: Mirror.ProductOf[Tuple.Elem[A, Pos]] =>
                derived[Tuple.Elem[A, Pos], Expecting](using ma)
              case _ =>
                error("Failed to derive or summon Bugged")
            }
        }

  case class Simplest(a: Int)
  case class Simple(a: Simplest)
  case class Advanced(a: Simple, b: Simplest)

  def usage[T](arg: T)(using MyTypeclass[T, Simplest]) =
    println(arg.doStuff(Simplest(1)))

  def example() =
    usage(Simple(Simplest(44))) // does compile
    usage(Advanced(Simple(Simplest(44)), Simplest(1))) //does not compile
    given proof: MyTypeclass[Simple, Simplest] = derived
    usage(Advanced(Simple(Simplest(44)), Simplest(1))) //does compile
  
end MyTypeclass