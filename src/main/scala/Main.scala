import slices.Slice
import subset.SubsetOf
import scala.compiletime.codeOf

object Main with

  def main(args: Array[String]): Unit = 
    val instance = Slice.derived[Large, Small]
    val large = Large(42, 43, "d2a")
    val small1 = instance.extractFrom(large)
    val small2 = small1.copy(b = 69)
    val large2 = instance.applyOn(large, small2)
    val message = SubsetOf.constMessage["a", ("a", "b", "c")]
    val positionA = SubsetOf.positionOf["c", ("a", "b", "c")]
    println(s"Large after applying small: $large2")
    println(s"Example detailed message: $message")
    println(s"Example value: $positionA")
    val positions = SubsetOf.allPositionsOf[("b", "a"), ("a", "b", "c", "d")]
    println(s"Positions of: $positions")
    val extracted = SubsetOf.extractSingleImpl[1, String, (Int, String)](1).apply((2, "3").toIArray)
    val advancedExtracted = SubsetOf.extractAll[(String, Int), (Int, String, Int), (1, 0)]((1, 0)).apply((1, "2", 3))
    println(s"Extracted values: ${extracted}")
    println(s"Extracted values: ${advancedExtracted.mkString(", ")}")

  def msg = "I was compiled by dotty :)"

  case class Large(a: Int, b: Int, c: String)
  case class Small(a: Int, b: Int)


