package parsermacro

object Client extends App {

  import Provider.For

  Provider.For#{x <- List(1, 2, 3)
                y <- (1 to 2 * x)
                z <- List(y + 1, y + 2, y + 3)}#{
                  // Will crash during macro expansion
                  //println(s"x = $x, y = $y, z = $z")
                  val sum = x + y + z
                  def square(x: Int) = x * x
                  println("x = " + x + ", y = " + y + ", z = " + z + ", sum = " + sum + ", square = " + square(sum))
                }

  val even =
    Provider.For#{x <- (1 to 10)
                  y <- if (x % 2 == 0) Some(x) else None}#{yield
                    def square(x: Int) = x * x
                    square(y)
                  }

  println("Squares of even numbers: " + even)

}
