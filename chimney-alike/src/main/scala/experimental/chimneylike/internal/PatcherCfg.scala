package experimental.chimneylike.internal

sealed abstract class PatcherCfg
object PatcherCfg:
  final class IgnoreRedundantPatcherFields extends PatcherCfg
  final class IgnoreNoneInPatch extends PatcherCfg
  final class OverwriteIterablesOnTheSameType extends PatcherCfg
end PatcherCfg

type EnablePatcherConfig[Config <: Tuple, Cfg <: PatcherCfg] <: Tuple = Config match
  case Cfg *: _ => Config
  case c *: config => c *: EnablePatcherConfig[config, Cfg]
  case EmptyTuple => Cfg *: EmptyTuple

type HasPatcherCfg[Config <: Tuple, Cfg <: PatcherCfg] <: Boolean = Config match
  case Cfg *: _ => true
  case EmptyTuple => false
  case c *: config => HasPatcherCfg[config, Cfg]
