package experimental.chimneylike

import experimental.chimneylike.dsl._
import experimental.chimneylike.internal.utils.MacroUtils
import experimental.chimneylike.internal._
import experimental.chimneylike.examples._
import utest._

object TransformerIntoSpec extends TestSuite:
  val tests = Tests {
      "TransformerInto" - {
        import TransformerCfg._
        import TransformerFlag._
        "builds correctly typed definition" - {
          val source = Source(a = 100, b = "100", c = "9000", d = 42L, e = 6.6)

          type ExpectedConfig = (FieldRelabelled["b", "c"], FieldComputedF["s"], FieldComputed["e"], FieldConstF["d"], FieldConst["b"], WrapperType[Option])
          type ExpectedFlags = (MethodAccessors, OptionDefaultsToNone)

          val transformation: TransformerFInto[Option, Source, Target, ExpectedConfig, ExpectedFlags] =
            source
              .into[Target]
              .withFieldConstF(_.s, Option.empty[Source])
              .withFieldConst(_.b, "200")
              .withFieldConstF(_.d, Some(420L))
              .withFieldComputed(_.e, source => (source.a + source.d + source.e) / 3)
              .withFieldComputedF(_.s, source => Some(source.copy(a = 200)))
              .withFieldRenamed(_.b, _.c)
              .enableOptionDefaultsToNone
              .enableMethodAccessors
          
          transformation.definition.overrides.get("b") ==> Some("200")
          transformation.definition.overrides("d") ==> Some(420L)
          transformation.definition.overrides("e")
            .asInstanceOf[Source => Double](source) ==> (source.a + source.d + source.e) / 3
          transformation.definition.overrides("s")
            .asInstanceOf[Source => Option[Source]](source) ==> Some(source.copy(a = 200))
          
        }
      }

      "TransformerDefinition" - {
        import TransformerCfg._
        import TransformerFlag._

        "builds correct transformer" - {

          val source = Source(a = 100, b = "100", c = "9000", d = 42L, e = 6.6)

          val transformer: Transformer[Source, Target] =
            defaultDefinition[Source, Target]
              .withFieldConst(_.b, "200")
              .withFieldComputed(_.e, source => (source.a + source.d + source.e) / 3)
              .withFieldComputed(_.s, source => source.copy(a = 200))
              .withFieldRenamed(_.b, _.c)
              .enableDefaultValues
              .buildTransformer
          
          transformer.transform(source) ==> Target(
            a = source.a, 
            b = "200", 
            c = "100",
            d = source.d, 
            e = (source.a + source.d + source.e) / 3,
            s = source.copy(a = 200),
            z = false
          )

        }

        "builds correct transformation recursively" - {
          val book = Book(
            "Average book", 
            List(
              Chapter("Nice chapter", 1, List("Lorem ipsum")),
              Chapter("Bad chapter", 2, List("The answer", "42"))
            )
          )

          val expectedReview = BookReview(
            "Average book",
            List(
              ChapterReview("Nice chapter", 1, "Lorem ipsum"),
              ChapterReview("Bad chapter", 2, "Lorem ipsum")
            ),
            "I liked it"
          )

          given transformer: Transformer[Book, BookReview] =
            defaultDefinition[Book, BookReview]
              .enableDefaultValues
              .withFieldConst(_.review, "I liked it")
              .buildTransformer

          book.transformTo ==> expectedReview
          
        }

        "builds correct transformer for coproducts" - {

          val book: LibraryObject = LibraryObject.Book("Nice book", List(Chapter("The beginning", 1, List("Lorem ipsum..."))))
          val newspaper: LibraryObject = LibraryObject.Newspaper("Nice newspaper", List("Lorem ipsum..."))
          val movie: LibraryObject = LibraryObject.Movie("Nice movie", Vector(1, 2, 3, 4))

          val indexedBook: IndexedObject = IndexedObject.Book("Nice book", List(IndexedChapter("The beginning", 1, List("Lorem ipsum..."))))
          val indexedNewspaper: IndexedObject = IndexedObject.Newspaper("Nice newspaper", List("Lorem ipsum..."))
          val indexedMovie: IndexedObject = IndexedObject.Movie("Nice movie", Vector(1, 2, 3, 4))

          given transformerFromIndex: Transformer[IndexedObject, LibraryObject] =
            defaultDefinition[IndexedObject, LibraryObject].buildTransformer

          given transformerToIndex: Transformer[LibraryObject, IndexedObject] =
            defaultDefinition[LibraryObject, IndexedObject]
              .enableDefaultValues
              .buildTransformer

          assert(
            book.transformTo == indexedBook,
            indexedBook.transformTo == book,
            newspaper.transformTo == indexedNewspaper,
            indexedNewspaper.transformTo == newspaper,
            movie.transformTo == indexedMovie,
            indexedMovie.transformTo == movie
          )
          
        }

      }

      "TransformerFDefinition" - {
        import TransformerCfg._
        import TransformerFlag._

        "builds correct transformer" - {

          val source = Source(a = 100, b = "100", c = "9000", d = 42L, e = 6.6)

          val transformer: TransformerF[Option, Source, Target] =
            defaultDefinition[Source, Target]
              .withFieldConstF(_.b, Option("200"))
              .withFieldComputed(_.e, source => (source.a + source.d + source.e) / 3)
              .withFieldComputed(_.s, source => source.copy(a = 200))
              .withFieldRenamed(_.b, _.c)
              .enableDefaultValues
              .buildTransformer
          
          transformer.transform(source) ==> Some(Target(
            a = source.a, 
            b = "200", 
            c = "100",
            d = source.d, 
            e = (source.a + source.d + source.e) / 3,
            s = source.copy(a = 200),
            z = false
          ))

        }

        "builds correct transformation recursively" - {
          val book = Book(
            "Average book", 
            List(
              Chapter("Nice chapter", 1, List("Lorem ipsum")),
              Chapter("Bad chapter", 2, List("The answer", "42"))
            )
          )

          val expectedReview = BookReview(
            "Average book",
            List(
              ChapterReview("Nice chapter", 1, "Lorem ipsum"),
              ChapterReview("Bad chapter", 2, "Lorem ipsum")
            ),
            "I liked it very much"
          )

          given transformer: TransformerF[Option, Book, BookReview] =
            defaultDefinition[Book, BookReview]
              .enableDefaultValues
              .withFieldConstF(_.review, Option("I liked it very much"))
              .buildTransformer

          book.transformTo ==> Some(expectedReview)
          
        }

      }
  }

  case class Source(a: Int, b: String, c: String, d: Long, e: Double)
  case class Target(a: Int, b: String, c: String, d: Long, e: Double = 4.4, s: Source, z: Boolean = false)

end TransformerIntoSpec