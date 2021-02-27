import slices.Slice
import scala.compiletime.codeOf

object Main with

  def main(args: Array[String]): Unit = 
    val instance = Slice.derived[Large, Small]
    val large = Large(42, 43, "d2a")
    val small1 = instance.extractFrom(large)
    val small2 = small1.copy(b = 69)
    val large2 = instance.applyOn(large, small2)
    println(s"Large after applying small: $large2")

  def msg = "I was compiled by dotty :)"

  case class Large(a: Int, b: Int, c: String)
  case class Small(a: Int, b: Int)


