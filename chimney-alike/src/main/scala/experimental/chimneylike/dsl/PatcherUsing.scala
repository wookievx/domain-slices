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
class PatcherUsing[T, P, C <: Tuple](val obj: T, val objPatch: P) extends PatcherDSL[T, P, C, [NC <: Tuple] =>> PatcherUsing[T, P, NC]]:

  /** Combines this configuration with next patch object (for efficient processing of complex data)
    * Any path from nextPatch will override changes from previous patches
    *
    * @param nextPatch next patch to process
    * @tparam NP type of patch object to add to configuration
    * @return [[PatcherUsingN]]
    */
  inline def and[NP](nextPatch: NP): PatcherUsingN[T, (P, NP), C] = PatcherUsingN(obj, (objPatch, nextPatch))

  /** Applies configured patching in-place
    *
    * @return patched value
    */
  inline def patch: T = PatcherDerive.derived[T, P, C, ""].patch(obj, objPatch)
end PatcherUsing

extension [T](obj: T)
  inline def using[P](patch: P): PatcherUsing[T, P, EmptyTuple] = PatcherUsing(obj, patch)

/** Provides operations to customize patcher logic for specific
  * object value and multiple patch values (for efficient data processing with patcher, no copying multiple updates at once).
  *
  * @param obj object to patch
  * @param objPatch patch objects
  * @tparam T type of object to apply patch to
  * @tparam PF type of patch object that dsl is focusing on
  * @tparam P type of patch objects (tuple)
  * @tparam C type-level encoded configuration of patcher
  */
class PatcherUsingN[T, P <: Tuple, C <: Tuple](val obj: T, patchers: P) extends PatcherDSL[T, P, C, [NC <: Tuple] =>> PatcherUsingN[T, P, NC]]:

  /** Combines this configuration with next patch object (for efficient processing of complex data)
    * Any path from nextPatch will override changes from previous patches
    *
    * @param nextPatch next patch to process
    * @tparam NP type of patch object to add to configuration
    * @return [[PatcherUsingN]]
    */
  inline def and[NP](nextPatch: NP): PatcherUsingN[T, Tuple.Concat[P, NP *: EmptyTuple], C] = PatcherUsingN(obj, patchers ++ (nextPatch *: EmptyTuple))

  /** Applies configured patching in-place
    *
    * @return patched value
    */
  inline def patch: T = PatcherDerive.derivedN[T, P, C].patch(obj, patchers)
end PatcherUsingN

trait PatcherDSL[T, P, C <: Tuple, DSL[_ <: Tuple]]:
  /** In case when both object to patch and patch value contain field
    * of type `Option[T]`, this option allows to treat `None` value in
    * patch like the value was not provided.
    *
    * By default, when `None` is delivered in patch, Chimney clears
    * the value of such field on patching.
    *
    * @see [[https://scalalandio.github.io/chimney/patchers/options-handling.html]] for more details
    * @return [[DSL]]
    */
  inline def ignoreNoneInPatch: DSL[EnablePatcherConfig[C, IgnoreNoneInPatch]] =
    this.asInstanceOf[DSL[EnablePatcherConfig[C, IgnoreNoneInPatch]]]

  /** In case that patch object contains a redundant field (i.e. field that
    * is not present in patched object type), this option enables ignoring
    * value of such fields and generate patch successfully.
    *
    * By default, when Chimney detects a redundant field in patch object, it
    * fails the compilation in order to prevent silent oversight of field name
    * typos.
    *
    * @see [[https://scalalandio.github.io/chimney/patchers/redundant-fields.html]] for more details
    * @return [[DSL]]
    */
  inline def ignoreRedundantPatcherFields: DSL[EnablePatcherConfig[C, IgnoreRedundantPatcherFields]] =
    this.asInstanceOf[DSL[EnablePatcherConfig[C, IgnoreRedundantPatcherFields]]]

  /** In case that patch object contains a field containing iterable of the same type element
    * as the field of object patched, this option enables making path override all values in patched object
    * (by default only N-first fields in iterable is modified, where N is size of the iterable from patch object)
    *
    * @return [[DSL]]
    */
  inline def overwriteIterablesOnTheSameType: DSL[EnablePatcherConfig[C, OverwriteIterablesOnTheSameType]] =
    this.asInstanceOf[DSL[EnablePatcherConfig[C, OverwriteIterablesOnTheSameType]]]

end PatcherDSL