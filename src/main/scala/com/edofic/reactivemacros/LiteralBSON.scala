package com.edofic.reactivemacros

import scala.language.experimental.macros

import util.parsing.combinator.JavaTokenParsers
import reactivemongo.bson._
import reflect.macros.Context

/**
 * User: andraz
 * Date: 3/12/13
 * Time: 9:14 AM
 */
object LiteralBSON  {
  def apply(s: String): BSONDocument = macro impl

  def impl(c: Context)(s: c.Expr[String]): c.Expr[BSONDocument] = {
    val h = new Helper[c.type](c)
    h.parse(s)
  }

  private class Helper[C <: Context](val c: C) extends JavaTokenParsers{
    import c.universe._

    def nakedString = stringLiteral ^^ (s => s.substring(1,s.length-1))

    def variable: Parser[c.Expr[BSONValue]] = """\$[a-zA-Z_]\w*""".r ^^ {
      id => c.Expr[BSONValue](
        Typed(Ident(id.substring(1)), btype("BSONValue"))
      )
    }

    def expression: Parser[c.Expr[BSONValue]] = """\$\$.*?\$\$""".r ^^ {
      exp => c.Expr[BSONValue](
        Typed(c.parse(exp.substring(2, exp.length -2)), btype("BSONValue"))
      )
    }

    def key: Parser[c.Expr[String]] = (ident | nakedString) ^^ {
      key => c.literal(key)
    }

    def string: Parser[c.Expr[BSONString]] = nakedString ^^ {
      string => c.Expr[BSONString]{
        Apply(bpath("BSONString"), List(c.literal(string).tree))
      }
    }

    def boolean: Parser[c.Expr[BSONBoolean]] = ("true" | "false") ^^ {
      boolean => c.Expr[BSONBoolean]{
        Apply(bpath("BSONBoolean"), List(c.literal(boolean).tree))
      }
    }

    def number: Parser[c.Expr[BSONValue]] = floatingPointNumber ^^ { number =>
      c.Expr[BSONValue]{
        util.Try{
          Apply(bpath("BSONInteger"), List(c.literal(number.toInt).tree))
        }.getOrElse{
          Apply(bpath("BSONDouble"), List(c.literal(number.toDouble).tree))
        }
      }
    }

    def value: Parser[c.Expr[BSONValue]] =
      number | string | boolean | document | expression | variable

    def keyVal: Parser[c.Expr[(String, BSONValue)]] = key ~ ":" ~ value ^^ {
      case key ~ _ ~ value => reify{ (key.splice, value.splice) }
    }

    def document: Parser[c.Expr[BSONDocument]] =
      "{" ~> repsep(keyVal, ",") <~ "}" ^^ {
        lst => c.Expr[BSONDocument](Apply(bpath("BSONDocument"), lst map (_.tree)))
      }

    def parse(s: c.Expr[String]): c.Expr[BSONDocument] = {
      val resetTree = c.Expr[String](c.resetAllAttrs(s.tree))
      val raw = util.Try(c.eval(resetTree)).getOrElse{
        c.abort(c.enclosingPosition, "BSON string must be known at compile time")
      }

      parseAll(document, raw) match {
        case Success(expr, _) => expr
        case NoSuccess(str, input) => c.abort(c.enclosingPosition, s"Error parsing BSON: \n$str\n$input")
      }
    }

    private def bpath(name: String) = Select(Select(Ident(newTermName("reactivemongo")), "bson"), name)

    private def btype(name: String) = Select(Select(Ident("reactivemongo"), "bson"), newTypeName(name))
  }
}


