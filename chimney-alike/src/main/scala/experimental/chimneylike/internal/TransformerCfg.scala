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
  final class WrapperType[F[+_], C <: TransformerCfg] extends TransformerCfg
end TransformerCfg

type EnableConfig[Config <: Tuple, Cfg <: TransformerCfg] = Cfg *: DisableConfig[Config, Cfg]

import TransformerCfg.*

type DisableConfig[Config <: Tuple, Cfg <: TransformerCfg] <: Tuple = (Config, Cfg) match
  case (FieldConst[a] *: tail, FieldConst[b]) => 
    (a != b) match
      case true => FieldConst[a] *: DisableConfig[tail, Cfg]
      case false => DisableConfig[tail, Cfg]
  case (FieldConstF[a] *: tail, FieldConstF[b]) => 
    (a != b) match
      case true => FieldConstF[a] *: DisableConfig[tail, Cfg]
      case false => DisableConfig[tail, Cfg]
  case (FieldComputedF[a] *: tail, FieldComputedF[b]) => 
    (a != b) match
      case true => FieldComputedF[a] *: DisableConfig[tail, Cfg]
      case false => DisableConfig[tail, Cfg]
  case (FieldRelabelled[a1, a2] *: tail, FieldRelabelled[b1, b2]) => 
    (a1 != b1 ) || (a2 != b2) match
      case true => FieldRelabelled[a1, a2] *: DisableConfig[tail, Cfg]
      case false => DisableConfig[tail, Cfg]
  case (_, _) => Config
