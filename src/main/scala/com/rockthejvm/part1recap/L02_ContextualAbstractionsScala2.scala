package com.rockthejvm.part1recap

object L02_ContextualAbstractionsScala2 {


  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }


  def increment(x: Int)(implicit amount: Int): Int = x + amount
  implicit val amnt: Int = 4

  object Implicits1 {

    // implicit classes
    case class Person1(name: String, age: Int)

    implicit class Show(person: Person1) {
      def show: String = s"showing: Person1[${person.name}, ${person.age}]"
    } // you create a new class and define a new interface
    // new interface becomes an extension method


    // implicit conversion
    implicit def str2len(name: String): Int = name.length
      // usually no new functionality added


    // implicit params (+ implicit vals)
    def show(person: Person1)(implicit prefix: String): String = prefix + s"name: ${person.name}, age: ${person.age}"


    def demonstrate1(): Unit =
      val alice: Person1 = Person1("alice", 32)
      println(alice.show) // because of implicit class

      def add(p1: Int, p2: Int): Int = p1 + p2

      println(add(2, "alice")) // should be 7 becaue of implicit conversion.

      // implicit params
      implicit val showPrefix: String = "Showing: "
      println(show(alice))

  }

  object TypeClasses {

    case class Employee(firstName: String, lastName: String, age: Int)

    trait JSONSerializer[T] {
      def toJson(value: T): String
    }

    implicit object IntJSONSerializer extends JSONSerializer[Int] {
      override def toJson(value: Int): String = s"\"int-value\" : \"${value.toString}\""
    }

    implicit object StringJSONSerializer extends JSONSerializer[String] {
      override def toJson(value: String): String = s"\"string-value\" : \"$value\""
    }

    implicit object EmployeeJSONSerializer extends JSONSerializer[Employee] {
      override def toJson(empl: Employee): String =
        s"""
           | \"empl\" : {
           |   \"first-name\" : \"${empl.firstName}\",
           |   \"last-name\" : \"${empl.lastName}\",
           |   \"age\" : \"${empl.age}\",
           | }""".stripMargin
    }

    implicit class ToJSONSerializer[T](value: T) {
      def toJson(implicit serializer: JSONSerializer[T]): String = serializer.toJson(value)
    }

    def demonstrateTypeClasses() : Unit = {
      val empl1 = Employee("John", "Connor", 32)
      println(empl1.toJson)
    }
  }


  def main(args: Array[String]): Unit = {

    implicit val amnt2: Int = 10

    println(Person("Paweł").greet)
    println("Paweł".greet)
    println(increment(3))

    import Implicits1._
    demonstrate1()

    import TypeClasses._
    demonstrateTypeClasses()

  }
}
