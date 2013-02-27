package com.edofic.reactivemacros

import reflect.macros.Context
import reactivemongo.bson.handlers.{BSONWriter, BSONReader}
import reactivemongo.bson.{BSONValue, BSONDocument}
import collection.mutable.ListBuffer

/**
 * User: andraz
 * Date: 2/21/13
 * Time: 6:51 PM
 */
private object MacroImpl {
  def read[A: c.WeakTypeTag](c: Context): c.Expr[BSONReader[A]] = {
    val body = readBody(c)

    c.universe.reify {
      new BSONReader[A] {
        def fromBSON(document: BSONDocument): A = body.splice
      }
    }
  }

  def write[A: c.WeakTypeTag](c: Context): c.Expr[BSONWriter[A]] = {
    val body = writeBody(c)
    c.universe.reify (
      new BSONWriter[A] {
        def toBSON(document: A): BSONDocument = body.splice
      }
    )
  }

  def format[A: c.WeakTypeTag](c: Context): c.Expr[BSONReader[A] with BSONWriter[A]] = {
    val r = readBody[A](c)
    val w = writeBody[A](c)
    c.universe.reify(
      new BSONReader[A] with BSONWriter[A] {
        def fromBSON(document: BSONDocument): A = r.splice

        def toBSON(document: A): BSONDocument = w.splice
      }
    )
  }

  private def readBody[A: c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._

    val (constructor, _) = matchingApplyUnapply[A](c)

    val values = constructor.paramss.head map {
      param =>
        val sig = param.typeSignature
        val optParam = optionTypeParameter(c)(sig)
        val typ = appliedType(readerType(c), List(optParam getOrElse sig))
        val reader = c.inferImplicitValue(typ)
        if (reader.isEmpty) c.abort(c.enclosingPosition, s"Implicit $typ for '$param' not found")

        val arg = Apply(Select(Ident(newTermName("map")), "apply"), List(Literal(Constant(param.name.toString))))
        val readExp = c.Expr[Nothing](Apply(Select(reader, "read"), List(arg)))
        val exp = if (optParam.isDefined) reify(
          try{
            Some(readExp.splice)
          } catch {
            case _: Exception => None
          }
        ) else readExp
        exp.tree
    }

    val constructorTree = c.parse(constructor.fullName)

    c.Expr[A](
      Block(
        ValDef(Modifiers(), newTermName("map"), TypeTree(), Select(Ident("document"), "mapped")),
        Apply(constructorTree, values)
      )
    )
  }

  private def writeBody[A: c.WeakTypeTag](c: Context): c.Expr[BSONDocument] = {
    import c.universe._

    val (constructor, deconstructor) = matchingApplyUnapply[A](c)
    val types = unapplyReturnTypes(c)(deconstructor)
    val constructorParams = constructor.paramss.head

    val tuple = Ident(newTermName("tuple"))
    val (optional, required) = constructorParams.zipWithIndex zip types partition (t=>isOptionalType(c)(t._2))
    val values = required map {
      case ((param, i), typ) => {
        val neededType = appliedType(writerType(c), List(typ))
        val writer = c.inferImplicitValue(neededType)
        if (writer.isEmpty) c.abort(c.enclosingPosition, s"Implicit $typ for '$param' not found")
        val tuple_i = if(types.length==1) tuple else Select(tuple, "_" + (i + 1))
        val bs_value = c.Expr[BSONValue](Apply(Select(writer, "write"), List(tuple_i)))
        val name = c.literal(param.name.toString)
        reify(
          (name.splice, bs_value.splice)
        ).tree
      }
    }

    val appends = optional map {
      case ((param, i), optType) => {
        val typ = optionTypeParameter(c)(optType).get
        val neededType = appliedType(writerType(c), List(typ))
        val writer = c.inferImplicitValue(neededType)
        if (writer.isEmpty) c.abort(c.enclosingPosition, s"Implicit $typ for '$param' not found")
        val tuple_i = c.Expr[Option[Any]](if(types.length==1) tuple else Select(tuple, "_" + (i + 1)))
        val buf = c.Expr[ListBuffer[(String,BSONValue)]](Ident("buf"))
        val bs_value = c.Expr[BSONValue](Apply(Select(writer, "write"), List(Select(tuple_i.tree, "get"))))
        val name = c.literal(param.name.toString)
        reify{
          if(tuple_i.splice.isDefined) buf.splice.append((name.splice, bs_value.splice))
        }.tree
      }
    }

    val companionTree = c.parse(companion[A](c).fullName)
    val document = Ident(newTermName("document"))
    val invokeUnapply = Select(Apply(Select(companionTree, "unapply"), List(document)), "get")
    val tupleDef = ValDef(Modifiers(), newTermName("tuple"), TypeTree(), invokeUnapply)
    val mkBSONdoc = Apply(bsonDocPath(c), values)


    c.Expr[BSONDocument](
      if(optional.length>0)
        Block(
          List(
            tupleDef,
            ValDef(Modifiers(), newTermName("bson"), TypeTree(), mkBSONdoc),
            c.parse("val buf = scala.collection.mutable.ListBuffer[(String,reactivemongo.bson.BSONValue)]()")
          ) ++ appends,
          c.parse("bson.append(buf: _*)")
        )
      else
        Block(tupleDef, mkBSONdoc)
    )
  }

  private def unapplyReturnTypes(c: Context)(deconstructor: c.universe.MethodSymbol): List[c.universe.Type] = {
    import c.universe._
    val opt = deconstructor.returnType match {
      case TypeRef(_, _, args) =>
        args.head match {
          case t@TypeRef(_, _, Nil) => Some(List(t))
          case typ@TypeRef(_, t, args) =>
            Some(
              if (t.name.toString.matches("Tuple\\d\\d?")) args else List(typ)
            )
          case _ => None
        }
      case _ => None
    }
    opt getOrElse c.abort(c.enclosingPosition, "something wrong with unapply type")
  }

  //Some(A) for Option[A] else None
  private def optionTypeParameter(c: Context)(typ: c.universe.Type): Option[c.universe.Type] = {
    import c.universe._
    if(isOptionalType(c)(typ))
      typ match {
        case TypeRef(_, _, args) => args.headOption
        case _ => None
      }
    else None
  }

  private def isOptionalType(c: Context)(typ: c.universe.Type): Boolean = {
    c.typeOf[Option[_]].typeConstructor == typ.typeConstructor
  }

  private def bsonDocPath(c: Context): c.universe.Select = {
    import c.universe._
    Select(Select(Ident(newTermName("reactivemongo")), "bson"), "BSONDocument")
  }

  private def applyMethod[A: c.WeakTypeTag](c: Context): c.universe.Symbol = {
    import c.universe._
    companion[A](c).typeSignature.declaration(stringToTermName("apply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
      case s => s
    }
  }

  private def unapplyMethod[A: c.WeakTypeTag](c: Context): c.universe.MethodSymbol= {
    import c.universe._
    companion[A](c).typeSignature.declaration(stringToTermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No unapply function found")
      case s => s.asMethod
    }
  }

  private def matchingApplyUnapply[A: c.WeakTypeTag](c: Context): (c.universe.MethodSymbol, c.universe.MethodSymbol) = {
    import c.universe._
    val applySymbol = applyMethod[A](c)
    val unapply = unapplyMethod[A](c)

    val alternatives = applySymbol.asTerm.alternatives map (_.asMethod)
    val u = unapplyReturnTypes(c)(unapply)
    val applys = alternatives filter (_.paramss.head.map(_.typeSignature) == u)

    val apply = applys.headOption getOrElse c.abort(c.enclosingPosition, "No matching apply/unapply found")
    (apply,unapply)
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
