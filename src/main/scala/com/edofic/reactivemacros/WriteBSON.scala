package com.edofic.reactivemacros

import reactivemongo.bson._
import handlers.BSONWriter
import language.experimental.macros
/**
 * User: andraz
 * Date: 2/21/13
 * Time: 6:11 PM
 */
trait WriteBSON[A]{
  def write(value: A): BSONValue
}

object WriteBSON {

  def apply[A]: BSONWriter[A] = macro MacroImpl.write[A]

  implicit val stringWriter = new WriteBSON[String] {
    def write(value: String): BSONValue = BSONString(value)
  }

  implicit val intWriter = new WriteBSON[Int] {
    def write(value: Int): BSONValue = BSONInteger(value)
  }
}
