package experimental.chimneylike.examples.complex
package updates

import DomainEventStuff._

enum ModifyDomainEvent:
  case PatchEvent(id: EventId, name: Option[EventName], markets: Map[MarketId, PatchMarket]) extends ModifyDomainEvent
  case UpdateEvent(id: EventId, name: EventName, markets: Map[MarketId, UpdateMarket]) extends ModifyDomainEvent
end ModifyDomainEvent

case class PatchMarket(name: Option[MarketName], selections: Map[SelectionId, PatchSelection])
case class PatchSelection(name: Option[SelectionName], result: Option[DomainSelectionResult], price: Option[Price])
case class UpdateMarket(id: MarketId, name: MarketName, selections: Map[SelectionId, Selection])
