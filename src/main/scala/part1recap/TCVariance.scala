package part1recap


object TCVariance {

  import cats.Eq
  import cats.instances.int._     // Eq[Int]
  import cats.instances.option._  // Eq[Option[Int]]
  import cats.syntax.eq._         // ===

  val aComparison = Option(2) === Some(2)
//  val invalidComparison = Some(1) === None

  // variance:
  class Animal
  class Cat extends Animal
  class Dog extends Animal
  // covariant:
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cats <: Animal , so Cage[Cat] <: Cage[Animal]

  // contravariant: subtyping is propagate BACKWARDS to the generic type:
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal]   // Cats <: Animal , so Vet[Cat] <: Vet[Animal]

  // contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]) = println("wow")
  makeSound[Animal] // ok
  makeSound[Cat] // ok

  sealed trait Printer[-A] {
    def print(a: A): String
    def printList(as: List[A]): String // List[+A] but its ok
    def prefixed(s: String): Printer[A]
    // variance
    def contramap[B](f: B => A): Printer[B]
  }

  sealed trait Coll[+A] {
    def apply(i: Int): A
    def map[B](f: A => B): Coll[B] // here A is on contravariant position and
  }

  final case class MultiMap[K, +V](items: Map[K, List[V]]) {
    def add[B >: V](key: K, value: B): MultiMap[K, B] = // B is a super type over V  "B super V"
      MultiMap(items + (key -> (items.get(key) match {
        case Some(values) => value :: values
        case None => List(value)
      })))
  }


  // implication for subtypes:
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]
//  makeSound[None]


  // covariant TC:
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = s"animal show"
  }
  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = s"cats show"
  }
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show


  def main(args: Array[String]): Unit = {
    println(aComparison)
    println(organizeShow[Cat]) // ok
    println(organizeShow[Animal](GeneralAnimalShow)) // only

    import cats.Eq
    import cats.syntax.eq._
    import cats.instances.int._
    import cats.instances.option._
    println(Option(2) === Option.empty[Int])
    println(Option(2) === None)
  }

}
