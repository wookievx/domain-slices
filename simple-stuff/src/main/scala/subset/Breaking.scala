package subset

case class GiantClass(
  a: Int,
  b: String,
  c: Double,
  d: List[String],
  e: String,
  f: String,
  g: (Int, String),
  h: Long,
  i: Map[Int, String],
  j: Short,
  k: Int = 0,
  l: Int = 0,
  m: Int = 0,
  n: Int = 0,
  o: Int = 1,
  p: Int = 1,
  r: Int = 1,
  s: Int = 1,
  t: Int = 2
)

case class GiantClassSubset[S](extractFn: GiantClass => S, applyFn: (GiantClass, S) => GiantClass) extends SubsetOf.SpecificSubsetOf[S, GiantClass](extractFn, applyFn)
object GiantClassSubset:
  inline def derived[S]: GiantClassSubset[S] = SubsetOf.deriveConcrete[S, GiantClass, GiantClassSubset[S]](GiantClassSubset.apply)
end GiantClassSubset

case class LargeClass(
  a: Int,
  d: List[String],
  g: (Int, String),
  h: Long,
  i: Map[Int, String],
  j: Short,
  t: Int
) derives GiantClassSubset

def workOnLarge[T](arg: T)(using LargeClass SubsetOf T): Unit =
  println(s"Started with: $arg")
  val update = arg.extract.copy(a = 42, j = 42)
  println(s"Extracted: ${update}")
  val res = arg.withSubset(update)
  println(s"Updated: ${res}")
  