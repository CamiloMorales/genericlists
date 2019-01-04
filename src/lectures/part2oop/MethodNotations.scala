package lectures.part2oop

object MethodNotations extends App {

  class Person(val name:String, val favoriteMovie:String, val age:Int = 0){
    def likes(movie:String):Boolean = movie == favoriteMovie
    def +(person:Person):String = s"${this.name}, what the heck?!"
    def +(nickName:String): Person = new Person(s"${this.name} ($nickName)", this.favoriteMovie)
    def unary_+ :Person = new Person(this.name, this.favoriteMovie, this.age + 1)
    def learns(something:String): String = s"${this.name} learns $something"
    def learnsScala : String = learns("Scala")
    def apply() : String = s"Hi my name is ${this.name} and I like ${this.favoriteMovie}"
    def apply(nTimes:Int): String = s"${this.name} watched ${this.favoriteMovie} $nTimes times"
  }

  val mary = new Person("Mary", "Braveheart")

  println(mary.likes("Braveheart"))
  println(mary likes "Braveheart") // Infix notation or operator notation

  //1. Overload + operator
  println((mary + "the Rockstar").apply)

  //2. Add age to the person class.
  //   Add a unary to increase the age by 1 and return a new Person.
  println(+mary age)

  //3. Add learns methods with String param.
  println(mary learnsScala)

  //4. Overload apply method
  print(mary apply 2)
}
