package com.edofic.reactivemacros

import language.experimental.macros
import reflect.macros.Context

/**
 * User: andraz
 * Date: 2/20/13
 * Time: 2:27 PM
 */
object Read {

  def apply[A](map: Map[String,Any]): A = macro impl[A]

  def impl[A: c.WeakTypeTag](c: Context)(map: c.Expr[Map[String,Any]]): c.Expr[A] = {
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

      val arg = Apply(Select(map.tree, "apply"), List(Literal(Constant(param.name.toString))))
      Apply(Select(reader, "read"), List(arg))
    }

    val constructorTree = c.parse(constructor.fullName)
    val a = Apply(constructorTree, values)

    val result = c.Expr[A](a)
    echo("Generated code\n"+show(result.tree))
    result
  }
}




