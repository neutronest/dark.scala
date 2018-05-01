package com.neulab.rand


//
//object RNGObject {
//}

class RNGZero(seed: Long) extends MyRNG {

//  def simple(seed: Long) : MyRNG = new MyRNG {
//    override def nextInt : (Int, MyRNG) = {
//      val seedNext = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) -1)
//      ((seedNext >>> 16).asInstanceOf[Int], simple(seedNext))
//    }
//  }

  def generateNextSeed(seed: Long) : Long = {
    (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) -1)
  }


  def randomPair() : (Int, Int) = {

    val (i1, _) = this.nextInt
    val (i2, _) = this.nextInt
    (i1, i2)
  }

  def randomPairV2(): ((Int, Int), RNGZero) = {

    val (r1, rng1) = this.nextInt
    val (r2, rng2) = rng1.nextInt
    ((r1, r2), rng2)

  }

  override def nextInt: (Int, RNGZero) = {
    val seedNext: Long = generateNextSeed(seed)
    ((seedNext >>> 16).asInstanceOf[Int], new RNGZero(seedNext))
  }

  def nextPositive: (Int, RNGZero) = {
    val seedNext: Long = generateNextSeed(seed)
    val intNext =  (seedNext >>> 16).asInstanceOf[Int]
    var positiveNext = 0
    if (intNext != Int.MinValue) {
      positiveNext = intNext.abs
    }
    (positiveNext,  new RNGZero(seedNext))
  }


  // TODO Implement
  def nextDouble: (Double, RNGZero) = {
    val seedNext: Long = generateNextSeed(seed)
    (1.0, new RNGZero(seedNext))
  }
}