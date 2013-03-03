package com.edofic.reactivemacros

/**
 * User: andraz
 * Date: 3/2/13
 * Time: 12:06 AM
 */
object Options {
  trait Default
  trait Verbose extends Default
  trait SaveClassName extends Default
  trait UnionType[Types <: \/[_,_]] extends SaveClassName with Default

  trait \/[A,B]
}
