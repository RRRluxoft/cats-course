package part1recap

object CatsIntro {

  // Eq
  val aComparison = 2 == "a string"

  // part 1
  import cats.Eq
  import cats.instances.int._
  import cats.instances.string._
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 5)

  import cats.syntax.eq._
  val anotherTypeSafeComparison = 2 === 3
  val neqComparison = 2 =!= 3
//  val invalidComparison = 2 === "5"


  // composite types:
  import cats.instances.list._ // Eq[List[Int]]
  val aListComparison = List(2) === List(3)

  // custom type:
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }
  val compareTwoToyCars = ToyCar("ford", 9.98) === ToyCar("ferrari", 9.98)



  import cats.implicits._
  import cats.effect.IO
  import cats.syntax._

  def fib(n: Long, a:Long = 0, b: Long = 1): IO[Long] =
    IO(a + b).flatMap { b2 =>
      if(n > 0) fib(n - 1, b, b2)
      else IO.pure(b2)
    }


  def main(args: Array[String]): Unit = {
    println(aTypeSafeComparison)
    println(s"fib of 5 : ${fib(5).unsafeRunSync()}")
    println(s"Compare toy car: ${compareTwoToyCars}")
  }


}
