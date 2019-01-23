package lectures.part2oop

import scala.annotation.tailrec

object Generics extends App {

  class MyListTemp[+A] {
    def add[B >: A](elem:B): MyListTemp[B] = ???

  }

  val listOfIntegers = new MyListTemp[Int]
  val listOfString = new MyListTemp[String]


/*
  object MyList {
    def empty[A]: MyListTemp[A] = ???
  }

  val emptyIntList = MyList.empty[Int]
*/

  class Animal
  class Cat extends Animal
  class Dog extends Animal

  class CovariantList[+A]

  val animal:Animal = new Cat
  val animalList:CovariantList[Animal] = new CovariantList[Cat]

  class Cage[A <: Animal](animal:A)
  val anotherCage = new Cage(new Animal)


  abstract class GenericMyList[+A] {
    def head: A
    def tail: GenericMyList[A]
    def isEmpty: Boolean
    def length: Int
    def add[B >: A](myListItem: B): GenericMyList[B]
    def prepend[B >: A](elem: B): GenericMyList[B]
    def printElements: String
    override def toString: String = s"[$printElements]"
    /*def map[B>:A,C](transformer:MyTransformer[B,C]):GenericMyList[C]
    def filter(predicate:MyPredicate[A]):GenericMyList[A]
    def flatMap[B>:A,C](transformer:MyTransformer[B,GenericMyList[C]]):GenericMyList[C]*/

    def map[B](transformer:A => B):GenericMyList[B]
    def flatMap[B](transformer:A => GenericMyList[B]):GenericMyList[B]
    def filter(predicate:A => Boolean):GenericMyList[A]
    def withFilter(q: A => Boolean): GenericMyList[A]

    def ++[B >: A](toAdd:GenericMyList[B]):GenericMyList[B]

    //def functionFilter(myRealFunction:(A => Boolean)):GenericMyList[A]
    //def functionMap[B>:A,C](myRealFunction:(B => C)):GenericMyList[C]

    def foreach(myFunction: A => Unit):Unit
    def sort(sortFunction: (A,A) => Int): GenericMyList[A]
    def zipWith[B >: A,C](list:GenericMyList[B], zipFunc:(B,B) => C): GenericMyList[C]
    def fold[B >: A](startElem:B)(foldFunc:(B,B) => B): B
   }

  case object EmptyGenericList extends GenericMyList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: GenericMyList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def length: Int = 0
    def add[B >: Nothing](myListItem: B): GenericMyList[B] = new ConsGeneric(myListItem, EmptyGenericList)
    def prepend[B >: Nothing](elem: B): GenericMyList[B] = new ConsGeneric(elem, EmptyGenericList)
    def printElements: String = ""

    /*def map[B >: Nothing, C](transformer: MyTransformer[B, C]): GenericMyList[C] = EmptyGenericList
    def filter(predicate: MyPredicate[Nothing]): GenericMyList[Nothing] = EmptyGenericList
    def flatMap[B >: Nothing, C](transformer: MyTransformer[B, GenericMyList[C]]): GenericMyList[C] = EmptyGenericList*/
    def ++[B >: Nothing](toAdd:GenericMyList[B]):GenericMyList[B] = toAdd

    def map[B](transformer:Nothing => B):GenericMyList[B] = EmptyGenericList
    def flatMap[B](transformer:Nothing => GenericMyList[B]):GenericMyList[B] = EmptyGenericList
    def filter(predicate:Nothing => Boolean):GenericMyList[Nothing] = EmptyGenericList
    def withFilter(predicate:Nothing => Boolean):GenericMyList[Nothing] = EmptyGenericList

    //def functionFilter(myRealFunction:(Nothing => Boolean)):GenericMyList[Nothing] = EmptyGenericList
    //def functionMap[B >: Nothing, C](myRealFunction: B => C): GenericMyList[C] = EmptyGenericList

    def foreach(myFunction: Nothing => Unit): Unit = ()
    def sort(sortFunction: (Nothing, Nothing) => Int): GenericMyList[Nothing] = EmptyGenericList
    def zipWith[B >: Nothing,C](list: GenericMyList[B], zipFunc: (B, B) => C): GenericMyList[C] =
      if(!list.isEmpty)
        throw new RuntimeException("Lists dont have the same length")
      else
        EmptyGenericList

    def fold[B >: Nothing](startElem: B)(foldFunc: (B, B) => B): B = startElem
  }

  case class ConsGeneric[+A](h:A, t:GenericMyList[A]) extends GenericMyList[A] {
    def head: A = h
    def tail: GenericMyList[A] = t
    def isEmpty: Boolean = false
    def length: Int = 1 + t.length
    def add[B >: A](myListItem: B): GenericMyList[B] = new ConsGeneric(h, t.add(myListItem))
    def prepend[B >: A](elem: B): GenericMyList[B] = new ConsGeneric[B](elem, this)
    def printElements: String = s"$h ${t.printElements}"

    /*def map[B>:A,C](transformer: MyTransformer[B, C]): GenericMyList[C] = {
      new ConsGeneric(transformer.transform(this.h), this.t.map(transformer))
    }*/

    def map[B](transformer:A => B):GenericMyList[B] = {
      new ConsGeneric(transformer(h), t.map(transformer))
    }

    /*def filter(predicate: MyPredicate[A]): GenericMyList[A] = {
        if(predicate.test(this.h))
          new ConsGeneric(this.h, t.filter(predicate))
        else
          t.filter(predicate)
    }*/

    def ++[B >: A](toAdd:GenericMyList[B]):GenericMyList[B] = new ConsGeneric(h, t ++ toAdd)

    /*def flatMap[B>:A,C](transformer: MyTransformer[B, GenericMyList[C]]): GenericMyList[C] = {
      transformer.transform(h) ++ t.flatMap(transformer)
    }*/

    def flatMap[B](transformer:A => GenericMyList[B]):GenericMyList[B] = {
      transformer(h) ++ t.flatMap(transformer)
    }

    def filter(myRealFunction:(A => Boolean)):GenericMyList[A] = {
      if(myRealFunction(h))
        new ConsGeneric(this.h, t.filter(myRealFunction))
      else
        t.filter(myRealFunction)
    }

    def withFilter(myRealFunction:(A => Boolean)):GenericMyList[A] = {
      if(myRealFunction(h))
        new ConsGeneric(this.h, t.filter(myRealFunction))
      else
        t.filter(myRealFunction)
    }

    def foreach(myFunction: A => Unit): Unit = {
      myFunction(h)
      t.foreach(myFunction)
    }


    def sort(sortFunction: (A, A) => Int): GenericMyList[A] = {
      @tailrec
      def insertOrderedElem[B >: A](originalList: GenericMyList[A],
                                    elem: B,
                                    sortFunction: (B, B) => Int,
                                    accumPreviousElems: GenericMyList[B]): GenericMyList[B] =
        if(originalList.isEmpty)
          new ConsGeneric(elem, EmptyGenericList)
        else if(sortFunction(elem,originalList.head) > 0)
          accumPreviousElems ++ new ConsGeneric(elem, originalList)
        else
          insertOrderedElem(originalList.tail, elem, sortFunction, accumPreviousElems.add(originalList.head))

      @tailrec
      def innerSort(current: GenericMyList[A], sortFunction: (A, A) => Int, sortedAccum:GenericMyList[A]): GenericMyList[A] = {
        if(current.isEmpty)
          sortedAccum
        else {
          innerSort(current.tail, sortFunction, insertOrderedElem(sortedAccum, current.head, sortFunction, EmptyGenericList))
        }
      }
      innerSort(this, sortFunction, EmptyGenericList)
    }

    def zipWith[B >: A,C](listToZipWith: GenericMyList[B], zipFunc: (B, B) => C): GenericMyList[C] = {
      @tailrec
      def innerZip(listA:GenericMyList[B],
                   listB:GenericMyList[B],
                   zipFunc: (B, B) => C,
                   accumZipList: GenericMyList[C]):GenericMyList[C] = {
        if(listA.isEmpty || listB.isEmpty)
          accumZipList
        else {
          innerZip(
            listA.tail,
            listB.tail,
            zipFunc,
            accumZipList.add(
              zipFunc(
                listA.head,
                listB.head
              )
            )
          )
        }
      }

      //Non-tail recursive version.  (shorter more readable version)
      def innerZip2()= {
        new ConsGeneric(zipFunc(h,listToZipWith.head), t.zipWith(listToZipWith.tail, zipFunc))
      }

      if(listToZipWith.length != this.length)
        throw new RuntimeException("Lists dont have the same length")
      else
        //innerZip(this,listToZipWith,zipFunc,EmptyGenericList)
        innerZip2
    }

    def fold[B >: A](startElem: B)(foldFunc: (B, B) => B): B = {
      @tailrec
      def innerFold(listToFold: GenericMyList[A], foldFunc: (B, B) => B, accumValue:B):B = {
        if(listToFold.isEmpty)
          accumValue
        else {
          innerFold(listToFold.tail, foldFunc, foldFunc(listToFold.head, accumValue))
        }
    }

      innerFold(this,foldFunc,startElem)
    }
  }

  //1
  trait MyPredicate[-T] {
    def test(elem:T):Boolean
  }

  class EvenPredicate extends MyPredicate[Int]{
    override def test(elem: Int): Boolean = {
      if(elem%2 == 0)
        true
      else
        false
    }
  }

  val myFunctionEvenPredicate: (Int => Boolean) = x => if(x % 2 == 0) true else false

  //2
  trait MyTransformer[-A,B] {
    def transform(elem:A):B
  }

  class Int2StringTransformer extends MyTransformer[Int, String]{
    def transform(elem: Int): String = {
      s"s${elem}"
    }
  }

  class Int2ListTransformer extends MyTransformer[Int, GenericMyList[String]]{
    def transform(elem: Int): GenericMyList[String] = {
      EmptyGenericList.add(s"s${elem}").add(s"s${elem+1}")
    }
  }

  val myFunctionStringTransformer:(Int => String) = x => s"s$x"

  //3


  //Test
  val newList = new ConsGeneric[Int](1, new ConsGeneric[Int](2, new ConsGeneric[Int](3, new ConsGeneric[Int](4, EmptyGenericList))))
  //println(s"Filter test: ${newList.filter(new EvenPredicate).toString}")
  //println(s"Map test: ${newList.map(new Int2StringTransformer).toString}")
  //println(s"FlatMap test: ${newList.flatMap(new Int2ListTransformer).toString}")

  println(s"Function Filter test: ${newList.filter(x => x % 2 == 0).toString}")
  println(s"Function Map test: ${newList.map((x:Int) => s"s$x").toString}")
  println(s"FlatMap test: ${newList.flatMap( x => EmptyGenericList.add(s"s${x}").add(s"s${x+1}")).toString}")



  val anotherList = new ConsGeneric[Int](1, new ConsGeneric[Int](3, new ConsGeneric[Int](5, new ConsGeneric[Int](7, EmptyGenericList))))
  println("Foreach test: ")
  anotherList.foreach(println)
  def orderFunc(elem1:Int, elem2:Int): Int = elem2-elem1
  val unorderedList = new ConsGeneric[Int](7, new ConsGeneric[Int](10, new ConsGeneric[Int](3, new ConsGeneric[Int](1, EmptyGenericList))))
  println(s"Sort test: ${unorderedList.sort(orderFunc).toString}")

  val zipListA = new ConsGeneric[Int](1, new ConsGeneric[Int](3, new ConsGeneric[Int](5, new ConsGeneric[Int](7, EmptyGenericList))))
  val zipListB = new ConsGeneric[Int](2, new ConsGeneric[Int](4, new ConsGeneric[Int](6, new ConsGeneric[Int](8, EmptyGenericList))))
  println(s"zipWith test: ${zipListA.zipWith(zipListB, (x:Int,y:Int) => x*y)}")

  println(s"fold test: ${zipListA.fold(0)((x:Int,y:Int) => x+y)}")

  //Supports For-comprenhension?
  for {
    curr1:Int <- zipListA
    curr2:Int <- zipListB

  } yield println(s"For-c: $curr1 - $curr2")














 /* val ml = EmptyGenericList

  println(ml.add(1).add(2).add(3).add(4).toString)
  println(ml.add(1).add(2).add(3).add(4).head)
  println(ml.add(1).add(2).add(3).add(4).tail)

  println(ml.prepend(1).prepend(2).prepend(3).prepend(4).toString)
  println(ml.prepend(1).prepend(2).prepend(3).prepend(4).head)
  println(ml.prepend(1).prepend(2).prepend(3).prepend(4).tail)



  println(ml.add("1").add("2").add("3").add("4").toString)
  println(ml.add("1").add("2").add("3").add("4").head)
  println(ml.add("1").add("2").add("3").add("4").tail)

  println(ml.prepend("1").prepend("2").prepend("3").prepend("4").toString)
  println(ml.prepend("1").prepend("2").prepend("3").prepend("4").head)
  println(ml.prepend("1").prepend("2").prepend("3").prepend("4").tail)*/
}


