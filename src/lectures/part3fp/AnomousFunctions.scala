package lectures.part3fp

object AnonymousFunctions extends App {

  val tripler: Int => Int = x => x * 3

  println(tripler(3) + 1)



}
