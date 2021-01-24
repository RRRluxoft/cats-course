package part1recap

object Implicits {

  // implicits classes:
  case class Person(name: String) {
    def greed: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greed: String = Person(name).greed
  }

  // implicit args and value:
  def incr(x:Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10

  //
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String = {
    list.map(e => serializer.toJson(e)).mkString("[", ",", "]")
  }

  // implicit methods:
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin
  }

  case class Cat(catName: String)


  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }


  def main(args: Array[String]): Unit = {
    // explicitly
    val impersonableString = new ImpersonableString("Tom")
    println(impersonableString.greed)

    val greeting = "Tom Waits".greed
    println(s"${greeting}")

    val incremented2 = incr(2) //(10)
    println(incremented2)

    implicit val serializer: JSONSerializer[Person] = new JSONSerializer[Person] {
      override def toJson(person: Person): String =
        s"""
           |{"name" : "${person.name}"}
           |""".stripMargin.trim
    }
    val personsJson = listToJson(List(Person("Cat"), Person("Tom")))
    println(s"Serializer: $personsJson")


    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Tom Cat")))
    println(oneArgCaseClassSerializer[Person].toJson(Person("Eddie")))

    println(listToJson(List(Cat("Tom2JSON"))) )//(oneArgCaseClassSerializer[Cat]))


    import JSONSyntax._
    val bob = Person("Bob")
    println(bob.toJson)

  }

}
