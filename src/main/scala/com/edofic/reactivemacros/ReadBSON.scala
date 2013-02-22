package com.edofic.reactivemacros

import reactivemongo.bson._
import language.experimental.macros
import reactivemongo.bson.handlers.BSONReader


/**
 * User: andraz
 * Date: 2/21/13
 * Time: 4:40 PM
 */
trait ReadBSON[A]{
  def read(value: BSONValue): A
}

trait ReadBSONimplicits {
  implicit def useExistingBSONWriter[A](implicit reader: BSONReader[A]) = new ReadBSON[A] {
    def read(value: BSONValue): A = reader.fromBSON(value.asInstanceOf[BSONDocument])
  }

  implicit val stringReader = new ReadBSON[String] {
    def read(value: BSONValue): String = value.asInstanceOf[BSONString].value
  }

  implicit val intReader = new ReadBSON[Int] {
    def read(value: BSONValue): Int = value.asInstanceOf[BSONInteger].value
  }
}

object ReadBSON extends ReadBSONimplicits {
  def apply[A]: BSONReader[A] = macro MacroImpl.read[A]
}