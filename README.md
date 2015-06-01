# parsermacro-example
Example of parser macros

Unfortunately this repo doesn't contain an sbt project yet because the plugin doesn't work in sbt at the moment.

```
master⭑1… parsermacro-example ❯ pmscalac Provider.scala && pmscalac Client.scala
warning: there were three unchecked warnings; re-run with -unchecked for details
one warning found
######################################################################
List(1, 2, 3).foreach(x => (1 to 2 * x).foreach(y => List(y + 1, y + 2, y + 3).foreach(z => {
  val sum = x + y + z
  def square(x: Int) = x * x
  println("x = " + x + ", y = " + y + ", z = " + z + ", sum = " + sum + ", square = " + square(sum))
})))
######################################################################
######################################################################
(1 to 10).flatMap(x => (if (x % 2 == 0) Some(x) else None).map(y => {
  def square(x: Int) = x * x
  square(y)
}))
######################################################################
master⭑1… parsermacro-example ❯ scala parsermacro.Client
x = 1, y = 1, z = 2, sum = 4, square = 16
x = 1, y = 1, z = 3, sum = 5, square = 25
x = 1, y = 1, z = 4, sum = 6, square = 36
(...)
x = 3, y = 6, z = 7, sum = 16, square = 256
x = 3, y = 6, z = 8, sum = 17, square = 289
x = 3, y = 6, z = 9, sum = 18, square = 324
Squares of even numbers: Vector(4, 16, 36, 64, 100)
```

Where `pmscalac` is an alias for:

```
master⭑1… parsermacro-example ❯ alias pmscalac
pmscalac='scalac -Xplugin:/Users/martin/.ivy2/cache/org.scalamacros/paradise_2.11.6/jars/paradise_2.11.6-2.1.0-M5.jar -Xplugin:/Users/martin/Documents/Projects/Duhemm/parsermacro/plugin/target/scala-2.11/fat-plugin.jar -cp /Users/martin/Documents/Projects/Duhemm/parsermacro/plugin/target/scala-2.11/fat-plugin.jar:sandbox:parsermacro:.'
```
