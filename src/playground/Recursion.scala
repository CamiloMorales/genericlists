package playground

import scala.annotation.tailrec

object Recursion extends App {

  //1. Concatenate a string n time with tail recursion


  def concatTailRec(n:Int, aString:String):String = {
    @tailrec
    def concatStringHelper(remaining:Int, aString:String, accumulator:String):String = {
      if(remaining==0)
        accumulator
      else
        concatStringHelper(remaining-1, aString, accumulator+aString)
    }

    concatStringHelper(n,aString,"")
  }

  println(concatTailRec(5, "Camilo"))


  //2. isPrime rail recursive

  def isPrimeTailRecursive(n:Int):Boolean = {
    @tailrec
    def isPrimerHelper(n:Int, remainingTests:Int, booleanAcc:Boolean):Boolean = {
      if(remainingTests==1 || !booleanAcc)
        booleanAcc
      else
        isPrimerHelper(n, remainingTests-1, (n%remainingTests != 0) && booleanAcc)
    }

    isPrimerHelper(n, n-1, true)
  }

  println(isPrimeTailRecursive(2003))
  println(isPrimeTailRecursive(629))

  //3. Fibonacci tail recursive
  //f0=0
  //f1=1
  //fn=fn-1+fn-2

  def fibonacciTailRec(n:Int):Int = {
    @tailrec
    def fibonacciTailRecHelper(n:Int, currentN:Int, fnMinus1:Int, fnMinus2:Int):Int = {
      if(currentN == n)
        fnMinus1+fnMinus2
      else
        fibonacciTailRecHelper(n,currentN+1,fnMinus1+fnMinus2, fnMinus1)
    }
    fibonacciTailRecHelper(n, 2, 1, 0)
  }

  println(fibonacciTailRec(8))
}
