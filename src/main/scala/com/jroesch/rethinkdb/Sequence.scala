package com.jroesch.rethinkdb

trait Sequence extends Query { //with Selection {
  /* enable for comphrensions here */
  def map = ???
  def flatMap = concatMap
  def concatMap = ???
  def orderBy = ???
  def skip = ???
  def limit = ???

  def apply(index: Long) = ???
  def apply(startIndex: Long, endIndex: Long) = ???

  def union(seq: Sequence): Sequence = ???

  def reduce(/* reduce function*/) = ???

  def count: Double = ???

  def distinct(seq: Sequence): Sequence = ???

  def pluck(attrs: Seq[String]): Sequence = ???

  def without = ???
}
