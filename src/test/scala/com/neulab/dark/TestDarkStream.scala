package com.neulab.dark

import org.scalatest.{FlatSpec, Matchers}
import com.neulab.dark.DarkStream

class TestDarkStream extends FlatSpec with Matchers {

  it should "test basic DarkStream" in {

    val emptyDarkStream = DarkStream.empty
    assert(emptyDarkStream.uncons.isEmpty)
  }

  it should "test DarkStream to DarkList" in  {
    val darkStreamSample = DarkStream.apply(List(1,2,3,4,5))
    assert(darkStreamSample.toDarkList == DarkList(1,2,3,4,5))
  }

  it should "test DarkStream take" in {

    val darkStreamSample = DarkStream.apply(List(1,2,3,4,5))
    assert(darkStreamSample.take(3).toDarkList == DarkList(1,2,3))
  }

}