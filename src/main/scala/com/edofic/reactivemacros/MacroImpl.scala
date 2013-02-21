package com.edofic.reactivemacros

import reflect.macros.Context
import reactivemongo.bson.handlers.{BSONWriter, BSONReader}
import reactivemongo.bson.{BSONValue, BSONDocument}

/**
 * User: andraz
 * Date: 2/21/13
 * Time: 6:51 PM
 */
private object MacroImpl {
  def read[A: c.WeakTypeTag](c: Context): c.Expr[BSONReader[A]] = {
    import c.universe._

    val constructor = applyMethod[A](c)

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

  def write[A: c.WeakTypeTag](c: Context): c.Expr[BSONWriter[A]] = {
    import c.universe._

    val deconstructor = unapplyMethod[A](c)

    val types = {
      deconstructor.returnType match {
        case TypeRef(_,_, args) =>
          args.head match {
            case t @ TypeRef(_, _, Nil) => Some(List(t))
            case TypeRef(_, _, args) => Some(args)
            case _ => None
          }
        case _ => None
      }
    } getOrElse c.abort(c.enclosingPosition, "something wrong with unapply type")

    val constructorParams = applyMethod[A](c).paramss.head

    if (constructorParams.length != types.length) c.abort(c.enclosingPosition, "apply/unapply don't match")

    val tuple = Ident(newTermName("tuple"))
    val values = constructorParams.zipWithIndex zip types map {
      case ((param,i), typ) => {
        val neededType = appliedType(writerType(c), List(typ))
        val writer = c.inferImplicitValue(neededType)
        val tuple_i = Select(tuple, "_" + (i + 1))
        val bs_value = c.Expr[BSONValue](Apply(Select(writer, "write"), List(tuple_i)))
        val name = c.literal(param.name.toString)
        reify (
          (name.splice, bs_value.splice)
        ).tree
      }
    }

    val companionTree = c.parse(companion[A](c).fullName)
    val document = Ident(newTermName("document"))
    val invokeUnapply = Select(Apply(Select(companionTree, "unapply"), List(document)), "get")
    val block = c.Expr[BSONDocument](
      Block(
        ValDef(Modifiers(), newTermName("tuple"), TypeTree(), invokeUnapply),
        Apply(bsonDocPath(c), values)
      )
    )

    val result = reify{
      new BSONWriter[A] {
        def toBSON(document: A): BSONDocument = block.splice
      }
    }

    c.echo(c.enclosingPosition, show(result))
    result
  }


  private def bsonDocPath(c: Context): c.universe.Select = {
    import c.universe._
    Select(Select(Ident(newTermName("reactivemongo")), "bson"), "BSONDocument")
  }

  private def applyMethod[A: c.WeakTypeTag](c: Context): c.universe.MethodSymbol = {
    import c.universe._
    companion[A](c).typeSignature.declaration(stringToTermName("apply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
      case s => s.asMethod
    }
  }

  private def unapplyMethod[A: c.WeakTypeTag](c: Context): c.universe.MethodSymbol= {
    import c.universe._
    companion[A](c).typeSignature.declaration(stringToTermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No unapply function found")
      case s => s.asMethod
    }
  }

  private def writerType(c: Context): c.Type = {
    c.typeOf[WriteBSON[_]].typeConstructor
  }

  private def readerType(c: Context): c.Type = {
    c.universe.typeOf[ReadBSON[_]].typeConstructor
  }

  private def companion[A: c.WeakTypeTag](c: Context): c.Symbol = {
    c.universe.weakTypeOf[A].typeSymbol.companionSymbol
  }
}
