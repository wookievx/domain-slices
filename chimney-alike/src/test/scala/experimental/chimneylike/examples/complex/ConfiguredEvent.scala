package experimental.chimneylike.examples.complex

object DomainEventStuff:
  opaque type EventId = Long
  opaque type EventName = String
  def eventId(long: Long): EventId = long
  def eventName(name: String): EventName = name

  opaque type MarketId = Long
  opaque type MarketName = String
  def marketId(long: Long): MarketId = long
  def marketName(name: String): MarketName = name

  opaque type SelectionId = Long
  def selectionId(long: Long): SelectionId = long
  opaque type SelectionName = String
  def selectionName(name: String): SelectionName = name

  opaque type Price = BigDecimal
  def price(value: BigDecimal): Price = value

end DomainEventStuff

import DomainEventStuff._

case class Event(id: EventId, name: EventName, markets: Map[MarketId, Market], config: String = "Default")
case class Market(id: MarketId, name: MarketName, selections: Map[SelectionId, Selection], config: String = "Default")
case class Selection(id: SelectionId, name: SelectionName, price: Price, result: Option[DomainSelectionResult])

enum DomainSelectionResult:
  case Won
  case Lost
  case Void
end DomainSelectionResult
