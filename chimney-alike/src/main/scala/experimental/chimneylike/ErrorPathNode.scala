package experimental.chimneylike

enum ErrorPathNode(val show: String, val separator: String):
  case Accessor(name: String) extends ErrorPathNode(show = name, separator = ".")
  case Index(value: Int) extends ErrorPathNode(show = s"($value)", separator = "")
  case MapValue(value: Any) extends ErrorPathNode(show = s"($value)", separator = "")
  case MapKey(key: Any) extends ErrorPathNode(show = s"($key)", separator = "")