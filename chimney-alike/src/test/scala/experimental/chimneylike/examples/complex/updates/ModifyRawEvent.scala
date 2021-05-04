package experimental.chimneylike.examples.complex
package updates

import RawEventStuff._

enum ModifyRawEvent:
  //patch event
  case PatchEvent(id: RawEventId, name: Option[RawEventName], markets: Map[RawMarketId, PatchRawMarket]) extends ModifyRawEvent
  //update event
  case UpdateEvent(id: RawEventId, name: RawEventName, markets: Map[RawMarketId, UpdateRawMarket]) extends ModifyRawEvent

end ModifyRawEvent

case class PatchRawMarket(name: Option[RawMarketName], selections: List[PatchRawSelection])
case class PatchRawSelection(id: RawSelectionId, name: Option[RawSelectionName], price: Option[Price], result: Option[SelectionResult])
case class UpdateRawMarket(id: RawMarketId, name: RawMarketName, selections: List[RawSelection])
