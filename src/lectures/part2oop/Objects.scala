package lectures.part2oop

object Objects extends App {

   class Person (name:String, age:Int) {
     def this(name:String) = {
       this(name, 0)
     }
   }

}