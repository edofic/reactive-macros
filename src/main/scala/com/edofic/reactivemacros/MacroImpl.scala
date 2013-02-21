package com.edofic.reactivemacros

import reflect.macros.Context
import reactivemongo.bson.handlers.BSONReader
import reactivemongo.bson.BSONDocument

/**
 * User: andraz
 * Date: 2/21/13
 * Time: 6:51 PM
 */
private object MacroImpl {
  def read[A: c.WeakTypeTag](c: Context): c.Expr[BSONReader[A]] = {
    import c.universe._

    val constructor = applyMethod(c)

    val values = constructor.paramss.head map { param =>
      val typ = appliedType(readerType(c), List(param.typeSignature))
      val reader = c.inferImplicitValue(typ)
      if(reader.isEmpty) c.abort(c.enclosingPosition, s"Implicit Reader for parameter '$param' not found")

      val arg = Apply(Select(Ident(newTermName("map")), "apply"), List(Literal(Constant(param.name.toString))))
      Apply(Select(reader, "read"), List(arg))
    }

    val constructorTree = c.parse(constructor.fullName)
    val creator = c.Expr[A](Apply(constructorTree, values))
    c.echo(c.enclosingPosition,"Generated code\n"+show(creator.tree))

    reify {
      new BSONReader[A] {
        def fromBSON(doc: BSONDocument): A = {
          val map = doc.mapped
          creator.splice
        }
      }
    }
  }

  def applyMethod[A: c.WeakTypeTag](c: Context) = {
    import c.universe._
    companion[A](c).typeSignature.declaration(stringToTermName("apply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
      case s => s.asMethod
    }
  }

  def readerType(c: Context): c.Type = {
    c.universe.typeOf[ReadBSON[_]].typeConstructor
  }

  def companion[A: c.WeakTypeTag](c: Context): c.Symbol = {
    c.universe.weakTypeOf[A].typeSymbol.companionSymbol
  }
}
