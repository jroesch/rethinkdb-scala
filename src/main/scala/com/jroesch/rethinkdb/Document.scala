package com.jroesch.rethinkdb

abstract class Document extends Queries#Query {
  def pluck = ???
  def without = ???
  def merge = ???
  def append = ???
  def apply(attr: String) = ???
  def contains = ???
}
