package lectures.part3fp

object WhatsAFunction extends App {

  val tripler: MyFunction[Int, Int] = new MyFunction[Int, Int] {
    override def apply(elem: Int): Int = elem * 3
  }

  println(tripler(3))

  val stringToInt: (String => Int)  = new Function1[String, Int] {
    override def apply(theStr:String): Int = theStr.toInt
  }

  println(stringToInt("222") + 3)

  trait MyFunction[A,B] {
    def apply(elem:A):B
  }

  //1. String concatenator
  val stringConcatenator: ((String, String) => String) = new Function2[String, String, String] {
    override def apply(v1: String, v2: String): String = v1 ++ (v2)
  }

  println(stringConcatenator("Scala", " Rocks"))

  //2. (Done in MyList)

  //3.
  val myFunctionReturningFunction: (Int => (Int => Int)) = new Function1[Int, Function1[Int, Int]] {
    override def apply(v1: Int): Int => Int = new Function1[Int, Int] {
      override def apply(v2: Int): Int = v1 + v2
    }
  }

  val retFunction: Int => Int = myFunctionReturningFunction(10)

  println(retFunction(20))
  println(myFunctionReturningFunction(10)(40))

  val specialAdder: Int => Int => Int = (x:Int) => (y:Int) => x+y

  println("specialAdder: " + specialAdder(5))
  println("specialAdder: " + specialAdder(10)(15))

}