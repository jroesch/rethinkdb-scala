package com.jroesch.rethinkdb

/* Phantom Type for typing Queries */
abstract class Database extends Queries#Query {
  /* Sub-Queries on a Database */
  def table = ???
  def tableCreate = ???
  def tableDrop = ???
  def tableList = ???
}
