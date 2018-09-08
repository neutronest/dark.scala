package com.neulab.rand

trait MyRNG {
  def nextInt[A]: (A, MyRNG)
  type Rand[+A] = MyRNG => (A, MyRNG)

  val int: Rand[Int] = _.nextInt

  def unit[A] (a: A): Rand[A] = myRng => (a, myRng)

  def map[A, B] (stateF: Rand[A]) (f: A => B) : Rand[B] = {
    myRng => {
      val (a, myRng2) = stateF(myRng)
      (f(a), myRng2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = {

    myRng => {

      val (a, rngA) = ra(myRng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }
  }


}

//type Rand[A+] = MyRNG => (A, MyRNG)