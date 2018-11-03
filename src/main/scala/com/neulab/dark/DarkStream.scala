package com.neulab.dark


trait DarkStream[+A] {
  def uncons: Option[(A, DarkStream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toDarkList: DarkList[A]
  def take(n: Int) : DarkStream[A]
}

object DarkStream {

  def empty[A]: DarkStream[A] = {
    new DarkStream[A] {
      override def uncons: Option[(A, DarkStream[A])] = None

      override def toDarkList: DarkList[A] = DarkNil

      override def take(n: Int): DarkStream[A] = empty
    }
  }


  def cons[A](x: => A, xs: => DarkStream[A]) : DarkStream[A] = {
    new DarkStream[A] {
      lazy val uncons = Some((x, xs))
      lazy val toDarkList = DarkCons(x, xs.toDarkList)

      override def take(n: Int): DarkStream[A] = {
        if (n == 0) empty
        else cons(x, xs.take(n-1))
      }
    }
  }

  // TODO: replace type with A*
  def apply[A](as: List[A]): DarkStream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail)) // why
  }
}


