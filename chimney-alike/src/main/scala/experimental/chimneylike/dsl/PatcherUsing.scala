package experimental.chimneylike.dsl

import experimental.chimneylike.internal.EnablePatcherConfig
import experimental.chimneylike.internal.PatcherCfg
import experimental.chimneylike.internal.PatcherCfg.*
import experimental.chimneylike.internal.derived.PatcherDerive

/** Provides operations to customize patcher logic for specific
  * object value and patch value.
  *
  * @param obj object to patch
  * @param objPatch patch object
  * @tparam T type of object to apply patch to
  * @tparam P type of patch object
  * @tparam C type-level encoded configuration of patcher
  */
class PatcherUsing[T, P, C <: Tuple](val obj: T, val objPatch: P):
  /** In case when both object to patch and patch value contain field
    * of type `Option[T]`, this option allows to treat `None` value in
    * patch like the value was not provided.
    *
    * By default, when `None` is delivered in patch, Chimney clears
    * the value of such field on patching.
    *
    * @see [[https://scalalandio.github.io/chimney/patchers/options-handling.html]] for more details
    * @return [[experimental.chimneylike.dsl.PatcherUsing]]
    */
  inline def ignoreNoneInPatch: PatcherUsing[T, P, EnablePatcherConfig[C, IgnoreNoneInPatch]] =
    this.asInstanceOf[PatcherUsing[T, P, EnablePatcherConfig[C, IgnoreNoneInPatch]]]

  /** In case that patch object contains a redundant field (i.e. field that
    * is not present in patched object type), this option enables ignoring
    * value of such fields and generate patch successfully.
    *
    * By default, when Chimney detects a redundant field in patch object, it
    * fails the compilation in order to prevent silent oversight of field name
    * typos.
    *
    * @see [[https://scalalandio.github.io/chimney/patchers/redundant-fields.html]] for more details
    * @return [[experimental.chimneylike.dsl.PatcherUsing]]
    */
  inline def ignoreRedundantPatcherFields: PatcherUsing[T, P, EnablePatcherConfig[C, IgnoreRedundantPatcherFields]] =
    this.asInstanceOf[PatcherUsing[T, P, EnablePatcherConfig[C, IgnoreRedundantPatcherFields]]]

  /** In case that patch object contains a field containing iterable of the same type element
    * as the field of object patched, this option enables making path override all values in patched object
    * (by default only N-first fields in iterable is modified, where N is size of the iterable from patch object)
    *
    * @return [[experimental.chimneylike.dsl.PatcherUsing]]
    */
  inline def overwriteIterablesOnTheSameType: PatcherUsing[T, P, EnablePatcherConfig[C, OverwriteIterablesOnTheSameType]] =
    this.asInstanceOf[PatcherUsing[T, P, EnablePatcherConfig[C, OverwriteIterablesOnTheSameType]]]

  /** Applies configured patching in-place
    *
    * @return patched value
    */
  inline def patch: T = PatcherDerive.derived[T, P, C, ""].patch(obj, objPatch)


end PatcherUsing

extension [T](obj: T)
  inline def using[P](patch: P): PatcherUsing[T, P, EmptyTuple] = PatcherUsing(obj, patch)