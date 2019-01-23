package lectures.part3fp

object MapFlatmapFilterFor extends App {
  val numbers = List(1,2,3,4)
  val chars = List('a','b','c','d')

  val res = chars.flatMap{
    x =>
      numbers.map{
        y =>
          s"$x$y"
    }
  }.toString

  println(res)

  val resFor =
    for {
      c <- chars
      n <- numbers
  } yield s"$c$n"

  println(resFor)
}
