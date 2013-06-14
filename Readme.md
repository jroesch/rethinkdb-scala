# A RethinkDB driver for Scala

A driver for RethinkDB written in Scala.

The goals of this driver is to have well-typed queries, that are easy to write, as well as providing an easy
to use typed interface for working with JSON data. 

The documentation, testing and code is still in progress. 

## Quickstart

```scala
import com.jroesch.{ rethinkdb => r }
import local._

r.table("users").run
```

There is a test connection exposed in local to allow you to connection on the default port, and host. The API mirrors
the one exposed in the Ruby/Python/JavaScript offical drivers.

You can write JSON 'literals' in this style.
```scala
val user = obj (
  "username" -> "jroesch", 
  "emails"   -> array("roeschinc@gmail.com", "jroesch@umail.ucsb.edu")
)
```

Allowing one to write things like:
```scala
r.table("users").insert(user)
```
