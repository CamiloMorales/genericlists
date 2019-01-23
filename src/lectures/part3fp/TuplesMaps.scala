package lectures.part3fp

object TuplesMaps extends App {

  trait SocialNetwork {
    def numberOfFriends(person:String):Int
    def personWithMostFriends:String
    def numberOfPersonsWithoutFriends:Int
  }

  class SocialNetworkImpl(val sn:Map[String, List[String]]) extends SocialNetwork {
    override def numberOfFriends(person: String): Int = {
      sn.get(person).getOrElse(List()).size
    }

    override def personWithMostFriends: String = {
      sn.foldLeft(("", 0)){
        (currentPair, nextPair) =>
          if(nextPair._2.size > currentPair._2 )
            (nextPair._1, nextPair._2.size)
          else
            currentPair
      }._1
    }

    override def numberOfPersonsWithoutFriends: Int = sn.filter(_._2.size == 0).size

  }

  println( new SocialNetworkImpl( Map("Camilo" -> List("Juan") , "Juan" -> List("Camilo", "Sebastian"), "Sebastian" -> List("Juan")) ).numberOfFriends("Juan"))
  println( new SocialNetworkImpl( Map("Camilo" -> List("Juan") , "Juan" -> List("Camilo", "Sebastian"), "Sebastian" -> List("Juan")) ).personWithMostFriends)
  println( new SocialNetworkImpl( Map("Camilo" -> List("Juan") , "Juan" -> List("Camilo"), "Sebastian" -> List(), "David" -> List())).numberOfPersonsWithoutFriends)

}
