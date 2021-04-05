package experimental.chimneylike.examples

case class Book(title: String, chapters: List[Chapter])

case class Chapter(title: String, number: Int, lines: List[String])

case class ReviewedBook(title: String, chapters: List[ChapterReview], review: String)

case class ChapterReview(title: String, number: Int, review: String = "Lorem ipsum")