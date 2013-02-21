package com.edofic.reactivemacros

import language.experimental.macros
import reflect.macros.Context
import reactivemongo.bson.{BSONDocument, BSONValue}
import reactivemongo.bson.handlers.BSONReader

/**
 * User: andraz
 * Date: 2/20/13
 * Time: 2:27 PM
 */
object Read {

  def apply[A]: BSONReader[A] = macro impl[A]

  def impl[A: c.WeakTypeTag](c: Context): c.Expr[BSONReader[A]] = {
    import c.universe._
    def echo(a: Any) = c.echo(c.enclosingPosition, a.toString)

    val companion = weakTypeOf[A].typeSymbol.companionSymbol

    val constructor = companion.typeSignature.declaration(stringToTermName("apply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
      case s => s.asMethod
    }

    val readerType = typeOf[ReadBSON[_]].typeConstructor

    val values = constructor.paramss.head map { param =>
      val typ = appliedType(readerType, List(param.typeSignature))
      val reader = c.inferImplicitValue(typ)
      if(reader.isEmpty) c.abort(c.enclosingPosition, s"Implicit Reader for parameter '$param' not found")

      val arg = Apply(Select(Ident(newTermName("map")), "apply"), List(Literal(Constant(param.name.toString))))
      Apply(Select(reader, "read"), List(arg))
    }

    val constructorTree = c.parse(constructor.fullName)
    val creator = c.Expr[A](Apply(constructorTree, values))
    echo("Generated code\n"+show(creator.tree))

    reify {
      new BSONReader[A] {
        def fromBSON(doc: BSONDocument): A = {
          val map = doc.mapped
          creator.splice
        }
      }
    }
  }
}




