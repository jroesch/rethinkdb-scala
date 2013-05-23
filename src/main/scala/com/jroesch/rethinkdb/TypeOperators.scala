package com.jroesch.rethinkdb

import language.implicitConversions

object TypeOperators {
  type Newtype[Repr, Ops] = { type Tag = NewtypeTag[Repr, Ops] }
  trait NewtypeTag[Repr, Ops]

  def newtype[Repr, Ops](r : Repr) : Newtype[Repr, Ops] = r.asInstanceOf[Any with Newtype[Repr, Ops]]

  implicit def newtypeOps[Repr, Ops](t : Newtype[Repr, Ops])(implicit mkOps : Repr => Ops) : Ops = t.asInstanceOf[Repr]
}
