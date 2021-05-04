package experimental.chimneylike.examples.complex

object RawEventStuff:
  opaque type RawEventId <: Long = Long
  opaque type RawEventName <: String = String
  def rawEventId(long: Long): RawEventId = long
  def rawEventName(name: String): RawEventName = name

  opaque type RawMarketId <: Long = Long
  opaque type RawMarketName <: String = String
  def rawMarketId(long: Long): RawMarketId = long
  def rawMarketName(name: String): RawMarketName = name

  opaque type RawSelectionId <: Long = Long
  opaque type RawSelectionName <: String = String
  
  def rawSelectionId(long: Long): RawSelectionId = long
  def rawSelectionName(name: String): RawSelectionName = name

  opaque type Price <: BigDecimal = BigDecimal

  def rawPrice(num: Int, den: Int): Price = BigDecimal(num.toDouble / den)

end RawEventStuff
import RawEventStuff._

case class RawEvent(name: RawEventName, id: RawEventId, markets: Map[RawMarketId, RawMarket])
case class RawMarket(name: RawMarketName, id: RawMarketId, selections: List[RawSelection])
case class RawSelection(name: RawSelectionName, id: RawSelectionId, price: Price, result: Option[SelectionResult])

enum SelectionResult:
  case Won
  case Lost
end SelectionResult