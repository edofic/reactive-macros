import com.edofic.reactivemacros.FormatBSON
import org.scalatest.FunSuite
import reactivemongo.bson.handlers.{BSONWriter, BSONReader}

/**
 * User: andraz
 * Date: 2/22/13
 * Time: 9:17 AM
 */
class MacroTest extends FunSuite{
  def roundtrip[A](original: A, format: BSONReader[A] with BSONWriter[A]) = {
    val serialized = format.toBSON(original)
    val deserialized = format.fromBSON(serialized)
    assert(original === deserialized)
  }

  test("nesting"){
    implicit val personFormat = FormatBSON[Person]
    val doc = Pet("woof", Person("john", "doe"))
    roundtrip(doc, FormatBSON[Pet])
  }

  test("primitives"){
    val doc = Primitives(1.2, "hai", true, 42, Long.MaxValue)
    roundtrip(doc, FormatBSON[Primitives])
  }

  test("seq"){
    val doc = WordLover("joe", Seq("hello", "world"))
    roundtrip(doc, FormatBSON[WordLover])
  }

  test("option"){
    val format = FormatBSON[Optional]
    val some = Optional("some", Some("value"))
    val none = Optional("none", None)
    roundtrip(some, format)
    roundtrip(none, format)
  }

  test("single member case class"){
    roundtrip(Single("FoO"), FormatBSON[Single])
  }

  test("single member option"){
    val f = FormatBSON[OptionalSingle]
    roundtrip(OptionalSingle(Some("foo")), f)
    roundtrip(OptionalSingle(None), f)
  }
}

case class Person(firstName: String, lastName: String)
case class Pet(name: String, owner: Person)
case class Primitives(dbl: Double, str: String, bl: Boolean, int: Int, long: Long)
case class WordLover(name: String, words: Seq[String])
case class Optional(name: String, value: Option[String])
case class Single(value: String)
case class OptionalSingle(value: Option[String])
case class SingleTuple(value: (String, String))