import com.edofic.reactivemacros.LiteralBSON
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

  test("parses stuff correctly"){
    val literal = LiteralBSON("""{hello: "world", "$get": 1.13, doc: {f: 2}, float: -1}""")
    val hand = BSONDocument(
      "hello" -> BSONString("world"),
      "$get" -> BSONDouble(1.13),
      "doc" -> BSONDocument("f" -> BSONInteger(2)),
      "float" -> BSONInteger(-1)
    )
    assert(mapped(literal) === mapped(hand))
  }
}
