package com.edofic.reactivemacros

import reactivemongo.bson.handlers.{BSONReader, BSONWriter}
import language.experimental.macros

/**
 * User: andraz
 * Date: 2/22/13
 * Time: 9:02 AM
 */
trait FormatBSON[A] extends ReadBSON[A] with WriteBSON[A]

trait FormatBSONimplicits extends ReadBSONimplicits with WriteBSONimplicits

object FormatBSON extends FormatBSONimplicits {
  def apply[A]: BSONReader[A] with BSONWriter[A] = macro MacroImpl.format[A, Options.Default]
  def custom[A, Opts]: BSONReader[A] with BSONWriter[A] = macro MacroImpl.format[A, Opts]
}
