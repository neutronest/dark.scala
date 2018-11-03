package com.neulab

import com.neulab.rand.RNGZero
import com.neulab.dark

object Main extends App {

//  println("Hello world")
//
//  val rng = new RNGZero(12356: Long)
//  val (i1, i2) = rng.randomPair()
//  println(i1, i2)
//
//  val (r1, r2) = rng.randomPairV2()
//  println(r1, r2)

  val l1 = List(1,2,3,4,5)
  val l2 = List("大", "波", "罗", "密", "谛")
  val implicitFoobar = dark.ImplicitFoobar
  val intRes : Int = implicitFoobar.sumV1(l1, dark.ImplicitFoobar.intImplicitFoobar)
  println("explicit int foobar", intRes)

  val stringRes = implicitFoobar.sumV1(l2, dark.ImplicitFoobar.stringImplicitFoobar)
  println("explicit string foobar ", stringRes)



  val res2 = implicitFoobar.sumV2(l1)
  println("implicit int foobar", res2)

  val res4 = implicitFoobar.sumV2(l2)
  println("implicit string foobar", res4)

  val res3 = implicitFoobar.sumV3(l1)(dark.ImplicitFoobar.intImplicitFoobar)
  println(res3)

  import implicitFoobar.intSleepWrapper
  5 times(println("Axiba"))


}

