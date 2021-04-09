package experimental.chimneylike.internal

import scala.compiletime.ops.any.!=
import scala.compiletime.ops.boolean._

sealed abstract class TransformerCfg
object TransformerCfg:
  final class FieldConst[Name <: String] extends TransformerCfg
  final class FieldConstF[Name <: String] extends TransformerCfg
  final class FieldComputed[Name <: String] extends TransformerCfg
  final class FieldComputedF[Name <: String] extends TransformerCfg
  final class FieldRelabelled[FromName <: String, ToName <: String] extends TransformerCfg
  final class CoproductInstance[InstType, TargetType] extends TransformerCfg
  final class CoproductInstanceF[InstType, TargetType] extends TransformerCfg
  final class WrapperType[F[_]] extends TransformerCfg
end TransformerCfg

type EnableConfig[Config <: Tuple, Cfg <: TransformerCfg] = Cfg *: DisableConfig[Config, Cfg]

import TransformerCfg._

type DisableConfig[Config <: Tuple, Cfg <: TransformerCfg] <: Tuple = Cfg match
  case FieldConst[name] => DisableField[Config, name]
  case FieldConstF[name] => DisableField[Config, name]
  case FieldComputed[name] => DisableField[Config, name]
  case FieldComputedF[name] => DisableField[Config, name]
  case FieldRelabelled[_, name] => DisableField[Config, name]
  case _ => Config

type DisableField[Config <: Tuple, Name <: String] <: Tuple = Config match
  case FieldConst[Name] *: tail => tail
  case FieldConstF[Name] *: tail => tail 
  case FieldComputed[Name] *: tail => tail
  case FieldComputedF[Name] *: tail => tail
  case FieldRelabelled[_, Name] *: tail => tail
  case h *: tail => h *: DisableField[tail, Name]
  case EmptyTuple => EmptyTuple
