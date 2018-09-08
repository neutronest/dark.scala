package com.neulab.myexhentai

import cats.Functor

abstract class MyLazyFunctor[F[_], A] { self =>

  def transformation[B] (f: A => B): F[B]

  def run: F[A] = transformation(identity)

  def map[B](f: A => B) : MyLazyFunctor[F, B] = new MyLazyFunctor[F, B] {
    def transformation[C](g: B => C): F[C] = self.transformation(g compose(f))
  }

  def toMyLazyFunctor[F[_], A](fa: F[A]) (implicit F: Functor[F]) : MyLazyFunctor[F, A] = new MyLazyFunctor[F, A] {
    override def transformation[B](f: A => B): F[B] = F.map(fa)(f)
  }

  def fromMyLazyFunctor[F[_], A](myLF: MyLazyFunctor[F, A]) : F[A] = myLF.run

  // Deprecated Code
  import cats.implicits._
  val lazyOption: MyLazyFunctor[Option, Int] = toMyLazyFunctor(Option(42))
  val transForOption : MyLazyFunctor[Option, Int] = lazyOption.map(_ + 1).map(_ + 2).map(_ + 3)
  val optionResult: Option[Int] = lazyOption.run
}
