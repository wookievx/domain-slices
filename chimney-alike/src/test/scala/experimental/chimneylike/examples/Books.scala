package experimental.chimneylike.examples

case class Book(title: String, chapters: List[Chapter])

case class Chapter(title: String, number: Int, lines: List[String])

case class BookReview(title: String, chapters: List[ChapterReview], review: String)

case class ChapterReview(title: String, number: Int, review: String = "Lorem ipsum")

case class AbstractBookReview(chapters: List[AbstractChapterReview], review: String)
case class AbstractChapterReview(review: String)