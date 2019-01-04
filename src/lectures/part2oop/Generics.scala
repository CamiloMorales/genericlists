package lectures.part2oop

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
    def add[B >: A](myListItem: B): GenericMyList[B]
    def prepend[B >: A](elem: B): GenericMyList[B]
    def printElements: String
    override def toString: String = s"[$printElements]"
    def map[B>:A,C](transformer:MyTransformer[B,C]):GenericMyList[C]
    def filter(predicate:MyPredicate[A]):GenericMyList[A]
    def flatMap[B>:A,C](transformer:MyTransformer[B,GenericMyList[C]]):GenericMyList[C]
    def ++[B >: A](toAdd:GenericMyList[B]):GenericMyList[B]

    def functionFilter(myRealFunction:(A => Boolean)):GenericMyList[A]
    def functionMap[B>:A,C](myRealFunction:(B => C)):GenericMyList[C]

    def foreach(myFunction: A => Unit):Unit
    def sort(sortFunction: (A,A) => Int): GenericMyList[A]
    def insertOrderedElem[B >: A](elem: B, sortFunction: (B, B) => Int): GenericMyList[B]
  }

  case object EmptyGenericList extends GenericMyList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: GenericMyList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[B >: Nothing](myListItem: B): GenericMyList[B] = new ConsGeneric(myListItem, EmptyGenericList)
    def prepend[B >: Nothing](elem: B): GenericMyList[B] = new ConsGeneric(elem, EmptyGenericList)
    def printElements: String = ""

    def map[B >: Nothing, C](transformer: MyTransformer[B, C]): GenericMyList[C] = EmptyGenericList
    def filter(predicate: MyPredicate[Nothing]): GenericMyList[Nothing] = EmptyGenericList
    def flatMap[B >: Nothing, C](transformer: MyTransformer[B, GenericMyList[C]]): GenericMyList[C] = EmptyGenericList
    def ++[B >: Nothing](toAdd:GenericMyList[B]):GenericMyList[B] = toAdd

    def functionFilter(myRealFunction:(Nothing => Boolean)):GenericMyList[Nothing] = EmptyGenericList
    def functionMap[B >: Nothing, C](myRealFunction: B => C): GenericMyList[C] = EmptyGenericList

    def foreach(myFunction: Nothing => Unit): Unit = myFunction
    def sort(sortFunction: (Nothing, Nothing) => Int): GenericMyList[Nothing] = EmptyGenericList

    override def insertOrderedElem[B >: Nothing](elem: B, sortFunction: (B, B) => Int): GenericMyList[B] =
      new ConsGeneric(elem, EmptyGenericList)
  }

  case class ConsGeneric[+A](h:A, t:GenericMyList[A]) extends GenericMyList[A] {
    def head: A = h
    def tail: GenericMyList[A] = t
    def isEmpty: Boolean = false
    def add[B >: A](myListItem: B): GenericMyList[B] = new ConsGeneric(h, t.add(myListItem))
    def prepend[B >: A](elem: B): GenericMyList[B] = new ConsGeneric[B](elem, this)
    def printElements: String = s"$h ${t.printElements}"

    def map[B>:A,C](transformer: MyTransformer[B, C]): GenericMyList[C] = {
      new ConsGeneric(transformer.transform(this.h), this.t.map(transformer))
    }

    def functionMap[B >: A, C](myRealFunction:(B => C)): ConsGeneric[C] = {
      new ConsGeneric(myRealFunction(h), t.functionMap(myRealFunction))
    }

    def filter(predicate: MyPredicate[A]): GenericMyList[A] = {
        if(predicate.test(this.h))
          new ConsGeneric(this.h, t.filter(predicate))
        else
          t.filter(predicate)
    }

    def ++[B >: A](toAdd:GenericMyList[B]):GenericMyList[B] = new ConsGeneric(h, t ++ toAdd)

    def flatMap[B>:A,C](transformer: MyTransformer[B, GenericMyList[C]]): GenericMyList[C] = {
      transformer.transform(h) ++ t.flatMap(transformer)
    }

    def functionFilter(myRealFunction:(A => Boolean)):GenericMyList[A] = {
      if(myRealFunction(h))
        new ConsGeneric(this.h, t.functionFilter(myRealFunction))
      else
        t.functionFilter(myRealFunction)
    }

    def foreach(myFunction: A => Unit): Unit = {
      myFunction(h)
      t.foreach(myFunction)
    }

    def insertOrderedElem[B >: A](elem: B, sortFunction: (B, B) => Int): GenericMyList[B] =
      if(sortFunction(elem,h) < 0)
        new ConsGeneric(elem, this)
       else
        new ConsGeneric(h, t.insertOrderedElem(elem, sortFunction))

    def sort(sortFunction: (A, A) => Int): GenericMyList[A] = {
      def innerSort(current: GenericMyList[A], sortFunction: (A, A) => Int, sortedAccum:GenericMyList[A]): GenericMyList[A] = {
        if(current.isEmpty)
          sortedAccum
        else {
          innerSort(current.tail, sortFunction, sortedAccum.insertOrderedElem(current.head, sortFunction))
        }
      }
      innerSort(this, sortFunction, EmptyGenericList)
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
  println(s"Filter test: ${newList.filter(new EvenPredicate).toString}")
  println(s"Map test: ${newList.map(new Int2StringTransformer).toString}")
  println(s"FlatMap test: ${newList.flatMap(new Int2ListTransformer).toString}")

  println(s"Function Filter test: ${newList.functionFilter(x => x % 2 == 0).toString}")
  println(s"Function Map test: ${newList.functionMap((x:Int) => s"s$x").toString}")



  val anotherList = new ConsGeneric[Int](1, new ConsGeneric[Int](3, new ConsGeneric[Int](5, new ConsGeneric[Int](7, EmptyGenericList))))
  println("Foreach test: ")
  anotherList.foreach(x => println(s"Foreach: $x"))

  def orderFunc(elem1:Int, elem2:Int): Int = if(elem1 < elem2) -1 else +1
  println(s"insertOrderedElem: ${anotherList.insertOrderedElem(2, orderFunc).toString}")

  val unorderedList = new ConsGeneric[Int](7, new ConsGeneric[Int](10, new ConsGeneric[Int](3, new ConsGeneric[Int](1, EmptyGenericList))))
  println(s"Sort test: ${unorderedList.sort(orderFunc).toString}")




























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


