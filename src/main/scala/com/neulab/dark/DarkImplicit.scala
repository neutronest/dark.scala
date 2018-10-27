package com.neulab.dark

abstract class ImplicitFoobar[A] {
  def add(x: A, y:A): A
  def unit() : A
}

object ImplicitFoobar {

  implicit val stringImplicitFoobar : ImplicitFoobar[String] = new ImplicitFoobar[String] {
    override def add(x: String, y: String): String = x concat y

    override def unit(): String = ""

  }

  implicit val intImplicitFoobar : ImplicitFoobar[Int] = new ImplicitFoobar[Int] {
    override def add(x: Int, y: Int): Int = x + y

    override def unit(): Int = 0
  }

  def sumV1[A](xs: List[A], m: ImplicitFoobar[A]) : A = {
    if (xs.isEmpty) {
      m.unit
    } else {
      m.add(xs.head, sumV1(xs.tail, m))
    }
  }

  def sumV2[A](xs: List[A])(implicit m: ImplicitFoobar[A]) : A = {
    if (xs.isEmpty) {
      m.unit
    } else {
      m.add(xs.head, sumV2(xs.tail))
    }
  }

  def sumV3[A](xs: List[A])(implicit m: ImplicitFoobar[A]) : A = {
    if (xs.isEmpty) {
      m.unit
    } else {
      m.add(xs.head, sumV2(xs.tail)(m))
    }
  }

  implicit class intSleepWrapper(x: Int) {
    def times[A](f: => A) : Unit = {
      def loop(current : Int) : Unit =
        if (current > 0) {
          f
          Thread.sleep(1000)
          loop(current-1)
        }
      loop(x)
    }
  }


}

