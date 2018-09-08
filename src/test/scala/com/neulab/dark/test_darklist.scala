package com.neulab.dark

import com.neulab.dark.DarkList
import org.scalatest._

class TestDarkList extends FlatSpec with Matchers {

    it should "darklist1 ++ darklist2 " in {
        
        val dl1 = DarkList(1,2,3)
        val dl2 = DarkList(4,5,6)

        assert(DarkList.++(dl1, dl2) == DarkList(1,2,3,4,5,6))
    }

    it should "reverse DarkList" in {
        val dl1 = DarkList(1,2,3,4,5,6)
        val dl2 = DarkList(6,5,4,3,2,1)
        assert(DarkList.reverse(dl1) == dl2)
    }

    it should "dropLast DarkList" in {
        val dl1 = DarkList(1,2,3,4,5,6)
        val dl2 = DarkList(1,2,3,4,5)
        assert(DarkList.dropLast(dl1) ==  dl2)
    }
}