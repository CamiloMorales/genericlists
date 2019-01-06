package lectures.part2oop

object Exceptions extends App {

  //1
  //val bigSeq : Seq[Double] = new Array[Double](Int.MaxValue)
  // println(bigSeq)

  //2
  /*def infiniteStringConcat(current:Double): String = {
    if(current >= Double.MaxValue)
      ""
    else
      infiniteStringConcat(current+1) + "Y OTRO! "
  }

  println(infiniteStringConcat(Double.MinValue))
  */

  //3
  object PocketCalculator {
    def add(x:Int, y:Int):Int = {
      val sum:Double = x.toDouble + y.toDouble
      if(sum > Int.MaxValue.toDouble)
        throw new OverflowException
      else
        sum.intValue()
    }

    def substract(x:Int, y:Int):Int = {
      val subs:Double = x.toDouble - y.toDouble
      if(subs < Int.MinValue.toDouble)
        throw new UnderflowException
      else
        subs.intValue()
    }

    def multiply(x:Int, y:Int):Int = {
      val prod:Double = x.toDouble * y.toDouble
      if(prod > Int.MaxValue.toDouble)
        throw new OverflowException
      else
        prod.intValue()
    }

    def divide(x:Int, y:Int):Int = {
      if(y == 0)
        throw new MathCalculationException
      else {
        val div:Double = x.toDouble / y.toDouble
        if(div > Int.MaxValue.toDouble)
          throw new OverflowException
        else if(div < Int.MinValue.toDouble)
          throw new UnderflowException
        else
          div.intValue()
      }
    }
  }

  class OverflowException extends RuntimeException
  class UnderflowException extends RuntimeException
  class MathCalculationException extends RuntimeException("Division by 0!")

  //println(PocketCalculator.add(Int.MaxValue, Int.MaxValue))
  //println(PocketCalculator.substract(Int.MinValue, 1))
  //println(PocketCalculator.multiply(Int.MaxValue, 2))
  println(PocketCalculator.divide(Int.MaxValue, 0))
}
