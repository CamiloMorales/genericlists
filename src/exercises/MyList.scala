package exercises

import scala.annotation.tailrec

abstract class MyList {
  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(myListItem: Int): MyList
  def preprend(elem: Int): MyList
  def printElements: String
  override def toString: String = s"[$printElements]"
}

object Empty extends MyList {
  override def head: Int = throw new NoSuchElementException
  override def tail: MyList = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add(elem: Int): MyList = new Cons(elem, Empty)
  override def preprend(elem: Int): MyList = new Cons(elem, Empty)
  override def printElements: String = "E"
}

class Cons(h:Int, t:MyList) extends MyList {
  override def head: Int = h
  override def tail: MyList = t
  override def isEmpty: Boolean = false
  override def add(elem: Int): MyList = new Cons(h, t.add(elem))
  override def preprend(elem: Int): MyList = new Cons(elem, this)
  override def printElements: String = s"$h ${t.printElements}"
}

object LetsTry extends App {
  val ml = Empty
  println(ml.add(1).add(2).add(3).add(4).toString)
  println(ml.add(1).add(2).add(3).add(4).head)
  println(ml.add(1).add(2).add(3).add(4).tail)
  println(ml.preprend(1).preprend(2).preprend(3).preprend(4).toString)
  println(ml.preprend(1).preprend(2).preprend(3).preprend(4).head)
  println(ml.preprend(1).preprend(2).preprend(3).preprend(4).tail)
}