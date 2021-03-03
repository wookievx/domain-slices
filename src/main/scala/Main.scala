import slices.Slice
import subset.*
import scala.compiletime.codeOf

object Main with

  def main(args: Array[String]): Unit = 
    workOnSmall(Large(1, 2, "42"))
    // given proof: SubsetOf[Small, Large] = SubsetOf.derive
    workRecursive(LargeRecurse(1, 2, Large(3, 4, "42")))

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
  case class Small(a: Int, b: Int)

  case class LargeRecurse(a: Int, b: Int, c: Large)
  case class SmallRecurse(a: Int, c: Small)


