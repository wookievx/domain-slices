import slices.Slice
import subset.*
import scala.compiletime.{codeOf, constValue}
import processes.Utils._
import util.DebugUtils

object Main:

  def main(args: Array[String]): Unit = 
    workOnSmall(Large(1, 2, "42"))
    workRecursive(LargeRecurse(1, 2, Large(3, 4, "42")))
    workOnLarge(GiantClass(
      a = 0,
      b = "0",
      c = 0,
      d = List("0"),
      e = "0",
      f = "0",
      g = (0, "0"),
      h = 0,
      i = Map(0 -> "0"),
      j = 0
    ))
    val arr = Array(1, 2, 3, 4, 5)
    DebugUtils.debug(arr.fastForeach(t => println(t)))    

  def msg = "I was compiled by dotty :)"


  def workOnSmall[T](arg: T)(using Small SubsetOf T): Unit = 
    val small = arg.extract
    small.copy(a = 420)
    println(s"Got small: $small")

  def workRecursive[T](arg: T)(using SmallRecurse SubsetOf T): Unit =
    val small = arg.extract
    println(s"Started with: $arg")
    val updated = small.copy(c = small.c.copy(b = 420))
    println(s"Got small (recursive): $small")
    val res = arg.withSubset(updated)
    println(s"Got large (recursive): $res, update: $updated")

  case class Large(a: Int, b: Int, c: String)
  case class Small(a: Int, b: Int) derives LargeSubset 

  case class LargeRecurse(a: Int, b: Int, c: Large)
  case class SmallRecurse(a: Int, c: Small) derives LargeRecursiveSubset

  case class LargeSubset[S](extractFn: Large => S, applyFn: (Large, S) => Large) extends SubsetOf.SpecificSubsetOf(extractFn, applyFn)
  object LargeSubset:
    inline def derived[S]: LargeSubset[S] = SubsetOf.deriveConcrete[S, Large, LargeSubset[S]](LargeSubset.apply)
  end LargeSubset

  case class LargeRecursiveSubset[S](extractFn: LargeRecurse => S, applyFn: (LargeRecurse, S) => LargeRecurse) extends SubsetOf.SpecificSubsetOf(extractFn, applyFn)
  object LargeRecursiveSubset:
    inline def derived[S]: LargeRecursiveSubset[S] = SubsetOf.deriveConcrete[S, LargeRecurse, LargeRecursiveSubset[S]](LargeRecursiveSubset.apply)
  end LargeRecursiveSubset

