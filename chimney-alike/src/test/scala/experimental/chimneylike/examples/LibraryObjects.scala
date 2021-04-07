package experimental.chimneylike.examples

enum LibraryObject(title: String):
  case Book(title: String, contents: List[Chapter]) extends LibraryObject(title)
  case Newspaper(title: String, articles: List[String]) extends LibraryObject(title)
  case Movie(title: String, data: Array[Byte]) extends LibraryObject(title)

enum IndexedObject(title: String, index: Int):
  case Book(title: String, contents: List[IndexedChapter], index: Int = 42) extends IndexedObject(title, index)
  case Newspaper(title: String, articles: List[String], index: Int = 42) extends IndexedObject(title, index)
  case Movie(title: String, data: Array[Byte], index: Int = 42) extends IndexedObject(title, index)

case class IndexedChapter(title: String, number: Int, lines: List[String], index: Int)