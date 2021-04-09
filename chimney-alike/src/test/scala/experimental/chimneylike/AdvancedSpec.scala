package experimental.chimneylike

import experimental.chimneylike.dsl._
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal._
import experimental.chimneylike.examples.complex._
import experimental.chimneylike.examples.complex.DomainEventStuff._
import experimental.chimneylike.examples.complex.RawEventStuff._
import experimental.chimneylike.examples.complex.updates._
import experimental.chimneylike.examples.complex.updates.Transformations._
import utest._

object AdvancedSpec extends TestSuite {

  val tests = Tests {
    "Library" - {

      "correctly transforms and patches updates" - {

        val initialState = Event(
          eventId(11),
          eventName("an event"),
          Map(
            marketId(1) -> Market(
              marketId(1),
              marketName("one"),
              Map(
                selectionId(1) -> Selection(
                  selectionId(1),
                  selectionName("first"),
                  price(BigDecimal(1)),
                  None
                ),
                selectionId(2) -> Selection(
                  selectionId(2),
                  selectionName("first"),
                  price(BigDecimal(1)),
                  None
                )
              ),
            ),
            marketId(2) -> Market(
              marketId(2),
              marketName("two"),
              Map(
                selectionId(1) -> Selection(
                  selectionId(1),
                  selectionName("first"),
                  price(BigDecimal(1)),
                  None
                ),
                selectionId(2) -> Selection(
                  selectionId(2),
                  selectionName("first"),
                  price(BigDecimal(1)),
                  None
                )
              ),
            )
          )
        )

        val updateOne: ModifyRawEvent.PatchEvent = ModifyRawEvent.PatchEvent(rawEventId(11), Some(rawEventName("the event")), Map.empty)
        val updateTwo: ModifyRawEvent.PatchEvent = ModifyRawEvent.PatchEvent(
          rawEventId(11),
          None,
          Map(
            rawMarketId(1) -> PatchRawMarket(
              Some(rawMarketName("patched one")),
              List(PatchRawSelection(rawSelectionId(1), None, None, Some(SelectionResult.Won)))
            )
          )
        )

        val updateRemoving: ModifyRawEvent.UpdateEvent = ModifyRawEvent.UpdateEvent(
          rawEventId(11),
          rawEventName("the event"),
          Map(
            rawMarketId(2) -> UpdateRawMarket(
              rawMarketId(2),
              rawMarketName("patched two"),
              List.empty
            )
          )
        )

        val resulting: Event = initialState
          .using(transformPatch(updateOne))
          .and(transformPatch(updateTwo))
          .and(transformUpdate(updateRemoving))
          .overwriteIterablesOnTheSameType
          .patch

        assert(
          resulting == Event(
            eventId(11),
            eventName("the event"),
            Map(
              marketId(1) -> Market(
                marketId(1),
                marketName("patched one"),
                Map(
                  selectionId(1) -> Selection(
                    selectionId(1),
                    selectionName("first"),
                    price(BigDecimal(1)),
                    Some(DomainSelectionResult.Won)
                  ),
                  selectionId(2) -> Selection(
                    selectionId(2),
                    selectionName("first"),
                    price(BigDecimal(1)),
                    None
                  )
                ),
              ),
              marketId(2) -> Market(
                marketId(2),
                marketName("patched two"),
                Map.empty,
              )
            )
          )
        )

      }

    }
  }

}
