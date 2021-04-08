package experimental.chimneylike.internal.derived

import scala.deriving.*
import scala.compiletime.*
import experimental.chimneylike.*
import experimental.chimneylike.internal.utils.MacroUtils

object PatcherDerive:
  import DeriveUtils.Concat

  inline def derived[T, P, Config <: Tuple, Path <: String]: Patcher[T, P] =
    summonFrom {
      case tm: Mirror.ProductOf[T] =>
        summonFrom {
          case pm: Mirror.ProductOf[P] =>
            PatcherDeriveProduct.deriveProduct[T, P, Config, Path](using tm, pm)
          case _ =>
            error(constValue["Requested patcher combination not supported, at: " Concat Path])
        }
      case tm: Mirror.SumOf[T] =>
        summonFrom {
          case pm: Mirror.SumOf[P] =>
            PatcherDeriveCoproduct.deriveCoproduct[T, P, Config, Path](using tm, pm)
          case _ =>
            error(constValue["Requested patcher combination not supported, at: " Concat Path])
        }
      case _ =>
        error(constValue["Requested patcher combination not supported, at: " Concat Path])
    }

end PatcherDerive
