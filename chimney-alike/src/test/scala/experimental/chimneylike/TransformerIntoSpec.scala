package experimental.chimneylike

import dsl.*
import internal.utils.MacroUtils
import internal.*
import utest.*
import examples.*

object TransformerIntoSpec extends TestSuite:
  val tests = Tests {
      "TransformerInto" - {
        import TransformerCfg.*
        import TransformerFlag.*
        "builds correctly typed definition" - {
          val source = Source(a = 100, b = "100", d = 42L, e = 6.6)

          type ExpectedConfig = (FieldComputedF["s"], FieldComputed["e"], FieldConstF["d"], FieldConst["b"], WrapperType[Option])
          type ExpectedFlags = (MethodAccessors, OptionDefaultsToNone)

          val transformation: TransformerFInto[Option, Source, Target, ExpectedConfig, ExpectedFlags] =
            source
              .into[Target]
              .withFieldConstF(_.s, Option.empty[Source])
              .withFieldConst(_.b, "200")
              .withFieldConstF(_.d, Some(420L))
              .withFieldComputed(_.e, source => (source.a + source.d + source.e) / 3)
              .withFieldComputedF(_.s, source => Some(source.copy(a = 200)))
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
        import TransformerCfg.*
        import TransformerFlag.*

        "builds correct transformer" - {

          val source = Source(a = 100, b = "100", d = 42L, e = 6.6)

          val transformer: Transformer[Source, Target] =
            defaultDefinition[Source, Target]
              .withFieldConst(_.b, "200")
              .withFieldComputed(_.e, source => (source.a + source.d + source.e) / 3)
              .withFieldComputed(_.s, source => source.copy(a = 200))
              .enableDefaultValues
              .buildTransformer
          
          transformer.transform(source) ==> Target(
            a = source.a, 
            b = "200", 
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

          val expectedReview = ReviewedBook(
            "Average book",
            List(
              ChapterReview("Nice chapter", 1, "Lorem ipsum"),
              ChapterReview("Bad chapter", 2, "Lorem ipsum")
            ),
            "I liked it"
          )

          given transformer: Transformer[Book, ReviewedBook] =
            defaultDefinition[Book, ReviewedBook]
              .enableDefaultValues
              .withFieldConst(_.review, "I liked it")
              .buildTransformer

          book.transformTo ==> expectedReview
          
        }

      }

      "TransformerFDefinition" - {
        import TransformerCfg.*
        import TransformerFlag.*

        "builds correct transformer" - {

          val source = Source(a = 100, b = "100", d = 42L, e = 6.6)

          val transformer: TransformerF[Option, Source, Target] =
            defaultDefinition[Source, Target]
              .withFieldConstF(_.b, Option("200"))
              .withFieldComputed(_.e, source => (source.a + source.d + source.e) / 3)
              .withFieldComputed(_.s, source => source.copy(a = 200))
              .enableDefaultValues
              .buildTransformer
          
          transformer.transform(source) ==> Some(Target(
            a = source.a, 
            b = "200", 
            d = source.d, 
            e = (source.a + source.d + source.e) / 3,
            s = source.copy(a = 200),
            z = false
          ))

        }

      }
  }

  case class Source(a: Int, b: String, d: Long, e: Double)
  case class Target(a: Int, b: String, d: Long, e: Double = 4.4, s: Source, z: Boolean = false)

end TransformerIntoSpec