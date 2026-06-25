package com.rockthejvm.part1recap

import com.rockthejvm.part1recap.L03_ContextualAbstractionsScala3.GivenUsing.Combiner
import scala.compiletime.{constValue, erasedValue}

object L03_ContextualAbstractionsScala3 {

  object GivenUsing {
    // given/using  > implicit params & implicit vals
    def increment(x: Int)(using amount: Int): Int = x + amount
    def multiply(x: Int)(using factor: Int): Int  = x * factor

    trait Combiner[T] {
      def combine(a: T, b: T): T
      def empty: T
    }

    def combine[T](list: List[T])(using combiner: Combiner[T]): T =
      list.foldLeft(combiner.empty)(combiner.combine)

    given Combiner[Int] with {
      override def combine(a: Int, b: Int): Int = a + b
      override def empty: Int                   = 0
    }

    given Combiner[String] with {
      override def combine(a: String, b: String): String = s"$a + $b"
      override def empty: String                         = "[]"
    }

    given optionIntCombiner(using intCombiner: Combiner[Int]): Combiner[Option[Int]] with {
      override def combine(a: Option[Int], b: Option[Int]): Option[Int] = for {
        va <- a.orElse(empty)
        vb <- b.orElse(empty)
      } yield intCombiner.combine(va, vb)
      override def empty: Option[Int]                                   = Some(intCombiner.empty)
    }

    def demonstrateGivenUsing(): Unit = {
      given Int       = 10
      val twelve: Int = increment(2)
      println(twelve)

      val three_hundred: Int = multiply(30)
      println(three_hundred)

      val someInts: List[Int]                 = (1 to 100).toList
      val someStrings: List[String]           = List("alpha", "beta", "gamma", "delta")
      val someOptionalInts: List[Option[Int]] = someInts.flatMap(x => List(Some(x), None))

      println(s"sum of 1 to 100: ${combine(someInts)}")
      println(s"concatenated strings: ${combine(someStrings)}")
      println(s"summing optional ints: ${combine(someOptionalInts)}")

    }
  }

  // extension  >

  object Extensions {

    case class Person(name: String) {
      def greet: String = s"Hi, I'm $name"
    }

    extension (name: String) {
      def greet: String = Person(name).greet
    }

    def demonstrateExtensions(): Unit = {
      println("Paweł".greet)
    }
  }

  object TypeClasses {

    case class Employee(firstName: String, lastName: String, age: Int)

    trait JSONSerializer[T] {
      def toJson(value: T): String
    }

    extension [T](element: T) {
      def toJson(using jsonSerializer: JSONSerializer[T]): String =
        jsonSerializer.toJson(element)
    }

    given s: JSONSerializer[Employee] with {
      override def toJson(employee: Employee): String =
        s"""
           |\"employee\" : {
           |  \"first-name\" : \"${employee.firstName}\",
           |  \"last-name\" : \"${employee.lastName}\",
           |  \"age\" : \"${employee.age}\",
           |}""".stripMargin
    }

    extension [T](list: List[T]) {
      def toJson(using jsonSerializer: JSONSerializer[T]): String = {
        list
          .map(_.toJson)
          .mkString("[\n\t", ",\n\t", "\n]")
      }
    }

    given JSONSerializer[Int] with {
      override def toJson(value: Int): String = s"\"value\" : \"$value\""
    }

    def demonstrateTC(): Unit = {
      println(Employee("John", "Connor", 32).toJson)
      println(List(1, 2, 3, 4).toJson)
    }
  }

  object Derived {

    import scala.deriving.Mirror

    trait JSONSerializer[T] {
      def toJson(value: T): String
    }

    object JSONSerializer {
      /*
      inline def derived[T](using m: Mirror.ProductOf[T]): JSONSerializer[T] = (value: T) => {
        val product: Product = value.asInstanceOf[Product]
        product.productIterator
          .map(v => s"""$v""")
          .mkString("[", ",", "]")
       }
       */

      inline def labels[T <: Tuple]: List[String] =
        inline erasedValue[T] match
          case _: EmptyTuple =>
            Nil
          case _: (h *: t)   =>
            constValue[h].toString :: labels[t]

      inline def derived[T](using m: Mirror.ProductOf[T]): JSONSerializer[T] = (value: T) =>
        val names: Seq[String] = labels[m.MirroredElemLabels]
        val values: Seq[Any] =
          value
            .asInstanceOf[Product]
            .productIterator
            .toList
        names
          .zip(values)
          .map { case (n, v) =>
            s""""$n":"$v""""
          }
          .mkString("{", ",", "}")
    }

    case class Employee(firstName: String, lastName: String, age: Int) derives JSONSerializer

    def demonstrateDerived(): Unit = {
      val empl1 = Employee("John", "Connor", 42)
      println(summon[JSONSerializer[Employee]].toJson(empl1))
    }
  }

  def main(args: Array[String]): Unit = {

    import GivenUsing._
    demonstrateGivenUsing()

    import Extensions._
    demonstrateExtensions()

    import TypeClasses._
    demonstrateTC()

    import Derived._
    demonstrateDerived()
  }

}
