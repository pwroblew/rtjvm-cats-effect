package com.rockthejvm.part1recap

object L04_CatsTypeClasses {
  
  
  
  // functor
  trait MyFunctor[F[_]] {
    def map[A, B](value: F[A])(f: A => B): F[B]
  }
  
  // applicative
  trait MyApplicative[F[_]] {
    def pure[A](value: A): F[A]
  }
  
  // flatMap
  trait FlatMap[F[_]] {
    def flatMap[A, B](value: A)(f: A => F[B]): F[B]
  }
  
  
  def main(args: Array[String]): Unit = {
    
  }
}
