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

  implicit val doubleReader = new ReadBSON[Double] {
    def read(value: BSONValue): Double = value.asInstanceOf[BSONDouble].value
  }

  implicit val stringReader = new ReadBSON[String] {
    def read(value: BSONValue): String = value.asInstanceOf[BSONString].value
  }

  implicit val booleanReader = new ReadBSON[Boolean] {
    def read(value: BSONValue): Boolean = value.asInstanceOf[BSONBoolean].value
  }

  implicit val intReader = new ReadBSON[Int] {
    def read(value: BSONValue): Int = value.asInstanceOf[BSONInteger].value
  }

  implicit val longReader = new ReadBSON[Long] {
    def read(value: BSONValue): Long = value.asInstanceOf[BSONLong].value
  }

  implicit def seqReader[A](implicit aReader: ReadBSON[A]) = new ReadBSON[Seq[A]] {
    def read(value: BSONValue): Seq[A] =
      value.asInstanceOf[BSONStructure].values.map(aReader.read).toSeq
  }
}

object ReadBSON extends ReadBSONimplicits {
  def apply[A]: BSONReader[A] = macro MacroImpl.read[A]
}