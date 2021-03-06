import com.edofic.reactivemacros._
import org.scalatest.FunSuite
import reactivemongo.bson._

/**
 * User: andraz
 * Date: 3/12/13
 * Time: 9:38 AM
 */
class ParserTest extends FunSuite{
  def mapped(doc: BSONDocument): Map[String, Any] = {
    val m = doc.mapped
    m mapValues {
      case d: BSONDocument => mapped(d)
      case v => v
    }
  }

  def compare(literal: BSONDocument, hand: AppendableBSONDocument) {
    assert(mapped(literal) === mapped(hand))
  }

  test("parses literals correctly"){
    val literal = LiteralBSON("""{hello: "world", "$get": 1.13, doc: {f: 2}, float: -1}""")
    val hand = BSONDocument(
      "hello" -> BSONString("world"),
      "$get" -> BSONDouble(1.13),
      "doc" -> BSONDocument("f" -> BSONInteger(2)),
      "float" -> BSONInteger(-1)
    )
  }

  test("single interpolated value"){
    val v = BSONString("hai")
    val lite = LiteralBSON("""{value:$v}""")
    val hand = BSONDocument("value" -> v)
    compare(lite, hand)
  }

  test("implicit conversions"){
    import WriteBSON.any2BSONValue
    val v = "hai"
    val lite = LiteralBSON("""{value:$v}""")
    val hand = BSONDocument("value" -> BSONString(v))
    compare(lite, hand)
  }

  test("expression"){
    import WriteBSON.any2BSONValue
    val lite = LiteralBSON("""{value: $$1+1$$}""")
    val hand = BSONDocument("value" -> BSONInteger(1+1))
    compare(lite, hand)
  }

  test("expression and a literal"){
    import WriteBSON.any2BSONValue
    val lite = LiteralBSON("""{value: $$1+1$$, name: "foo"}""")
    val hand = BSONDocument(
      "value" -> BSONInteger(1+1),
      "name" -> BSONString("foo")
    )
    compare(lite, hand)
  }
}
