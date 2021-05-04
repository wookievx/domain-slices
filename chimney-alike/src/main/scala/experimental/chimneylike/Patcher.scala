package experimental.chimneylike

import experimental.chimneylike.internal.derived.PatcherDerive

/** Type class definition that wraps patching behavior.
  *
  * @tparam T type of object to apply patch to
  * @tparam Patch type of patch object
  */
trait Patcher[T, Patch]:
  def patch(obj: T, patch: Patch): T
  extension (obj: T)
    inline def applyPatch(p: Patch): T = patch(obj, p)
end Patcher

object Patcher:
  inline def derived[T, Patch]: Patcher[T, Patch] = PatcherDerive.derived[T, Patch, EmptyTuple, ""]
end Patcher