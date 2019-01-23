package lectures.part3fp

object HOFsCurries extends App {


  val weirdFunction: (Int, (String, (Int => Boolean)) => Int) => (Int => Int) = new Function2[Int, Function2[ String, Function1[Int, Boolean], Int], Function1[Int, Int]] {
    override def apply(v1: Int, v2: (String, Int => Boolean) => Int): Int => Int = ???
  }

  val doubler: Int => Int = _ * 2

  def nTimes1( f: Int => Int, n:Int, x:Int) : Int = {
    if(n == 1) f(x) else nTimes(f, n-1, f(x))
  }

  val nTimes: ( ( Int => Int, Int, Int) => Int) = {
    (f,n,x) => if(n == 1) f(x) else nTimes(f, n-1, f(x))
  }

  println(nTimes1(doubler, 2, 20))
  println(nTimes(doubler, 2, 10))

  val plusOne: Int => Int = _ + 1

  def plusOneNTimes(f:Int=>Int, n:Int): Int=>Int = {
    x:Int =>
      if(n==0)
        x
      else
        plusOneNTimes(f, n-1)(f(x))
  }

  val plus6 = plusOneNTimes(plusOne, 6)

  println(plus6(20))


  //Curried functions

  val superAdder: Function1[Int, Function1[Int, Int]] = (x:Int) => (y:Int) => x+y


  def toCurry(f: (Int,Int) => Int): Int => Int => Int = {
    (x:Int) =>
      (y:Int) =>
        f(x,y)
  }

  println(toCurry( (x:Int,y:Int) => x+y )(5)(6))

  def fromCurry(f:Int => Int => Int): (Int,Int) => Int = {
    (x:Int, y:Int) =>
      f(x)(y)
  }

  println(fromCurry( (x:Int) => (y:Int) => x+y )(5,6))

  //3 Compose
  def compose(f: Int => Int, g: Int => Int): Int => Int = {
    (x:Int) => f(g(x))
  }

  println(compose((x:Int) => x*2, (y:Int) => y+3)(10))

  def andThen(f: Int => Int, g: Int => Int): Int => Int = {
    (x:Int) => g(f(x))
  }

  println(andThen((x:Int) => x*2, (y:Int) => y+3)(10))

}
