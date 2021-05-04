package experimental.chimneylike.examples.complex
package updates

import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.dsl._
import experimental.chimneylike._
import DomainEventStuff._
import RawEventStuff._

object Transformations:

  given Transformer[RawEventId, EventId] with
    def transform(from: RawEventId): EventId = eventId(from)

  given Transformer[RawEventName, EventName] with
    def transform(from: RawEventName): EventName = eventName(from)

  given Transformer[RawMarketId, MarketId] with
    def transform(from: RawMarketId): MarketId = marketId(from)

  given Transformer[RawMarketName, MarketName] with
    def transform(from: RawMarketName): MarketName = marketName(from)

  given Transformer[RawSelectionId, SelectionId] with
    def transform(from: RawSelectionId): SelectionId = selectionId(from)

  given Transformer[RawEventStuff.Price, DomainEventStuff.Price] with
    def transform(from: RawEventStuff.Price): DomainEventStuff.Price = price(from)

  given selectionTransformer: Transformer[List[RawSelection], Map[SelectionId, Selection]] with
    def transform(from: List[RawSelection]): Map[SelectionId, Selection] =
      from.view.map(s => selectionId(s.id) -> convertSelection(s)).toMap

  private def convertSelection(raw: RawSelection): Selection = {
    raw.into[Selection].withFieldComputed(_.name, rs => selectionName(rs.name)).transform
  }

  given patchTransformer: Transformer[List[PatchRawSelection], Map[SelectionId, PatchSelection]] with
    def transform(from: List[PatchRawSelection]): Map[SelectionId, PatchSelection] =
      from.view.map(s => selectionId(s.id) -> convertSelection(s)).toMap

  private def convertSelection(raw: PatchRawSelection): PatchSelection = {
    raw.into[PatchSelection].withFieldComputed(_.name, rs => rs.name.map(selectionName)).transform
  }

  def transformPatch(raw: ModifyRawEvent.PatchEvent): ModifyDomainEvent.PatchEvent =
    raw.into[ModifyDomainEvent.PatchEvent].enableDefaultValues.transform

  def transformUpdate(raw: ModifyRawEvent.UpdateEvent): ModifyDomainEvent.UpdateEvent =
    raw.into[ModifyDomainEvent.UpdateEvent].enableDefaultValues.transform

  def transformEvent(raw: RawEvent): Event = raw.into[Event].enableDefaultValues.transform


end Transformations
