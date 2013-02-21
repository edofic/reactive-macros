package com.edofic.reactivemacros

/**
 * User: andraz
 * Date: 2/21/13
 * Time: 4:40 PM
 */
trait Reader[A]{
  def read(a: Any): A
}

object Reader{
  def cast[A] = new Reader[A]{
    def read(a: Any): A = a.asInstanceOf[A]
  }
}