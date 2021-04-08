package experimental.chimneylike

import experimental.chimneylike.dsl.*
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal.*
import experimental.chimneylike.examples.*
import utest.*

object PatcherSpec extends TestSuite {
  val tests = Tests {
    "PatcherUsing" - {
      import PatcherCfg.*
      "correctly applies changes in products" - {
        def mockReview(n: Int) = ChapterReview(s"placeholder$n", n, "lorem ipsum dolor sit amet")

        def actualChapter(n: Int) = Chapter(s"Chapter: $n", n, lines = List("I am lazy, did not read that"))

        def properReview(n: Int) = ChapterReview(s"Chapter: $n", n, "lorem ipsum dolor sit amet")

        val target = BookReview("placeholder", List.range(1, 20).map(mockReview), "lorem ipsum dolor sit amet")

        val updates = Book("Long book", List.range(1, 20).map(actualChapter))

        val patched = target.using(updates).ignoreRedundantPatcherFields.patch
        assert(patched == BookReview("Long book", List.range(1, 20).map(properReview), "lorem ipsum dolor sit amet"))
      }

      "correctly applies changes in coproducts" - {
        def indexedChapter(n: Int) = IndexedChapter(s"Chapter: $n", n, List.fill(100)("lorem ipsume dolor sit amet"), index = 2 * n)
        def chapterUpdate(n: Int) = Chapter(s"Chapter: $n-updated", n + 10, List.range(1, 20).map(n => s"line $n, nothing to see here"))
        def updatedChapter(n: Int) =
          IndexedChapter(
            s"Chapter: $n-updated",
            n + 10,
            List.range(1, 20).map(n => s"line $n, nothing to see here"),
            index = 2 * n
          )

        val testBook: IndexedObject = IndexedObject.Book("Indexed book", List.range(1, 20).map(indexedChapter), index = 1)
        val testNewspaper: IndexedObject = IndexedObject.Newspaper("Indexed newspaper", List("article1", "article2"), index = 2)
        val testMovie: IndexedObject = IndexedObject.Movie("Indexed movie", Vector.empty, index = 3)

        val updateBook: LibraryObject = LibraryObject.Book("Indexed book", List(chapterUpdate(1), chapterUpdate(2)))
        val updateNewspaper: LibraryObject = LibraryObject.Newspaper("Indexed newspaper", List("article2", "article3"))
        val updateMovie: LibraryObject = LibraryObject.Movie("Indexed movie", Vector(1, 2, 3, 4))

        assert(
          testBook.using(updateBook).patch ==
            IndexedObject.Book("Indexed book", List(updatedChapter(1), updatedChapter(2)) ++ List.range(3, 20).map(indexedChapter), index = 1),
          testBook.using(updateBook).overwriteIterablesOnTheSameType.patch ==
            IndexedObject.Book("Indexed book", List(updatedChapter(1), updatedChapter(2)) ++ List.range(3, 20).map(indexedChapter), index = 1),
          testNewspaper.using(updateNewspaper).patch ==
            IndexedObject.Newspaper("Indexed newspaper", List("article2", "article3"), index = 2),
          testMovie.using(updateMovie).patch ==
            IndexedObject.Movie("Indexed movie", Vector(1, 2, 3, 4), index = 3)
        )
      }
    }
  }
}
