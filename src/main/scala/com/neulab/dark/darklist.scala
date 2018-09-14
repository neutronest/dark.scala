package com.neulab.dark

sealed trait DarkList[+A]
case object DarkNil extends DarkList[Nothing]
case class DarkCons[+A](head: A, tail: DarkList[A]) extends DarkList[A]

object DarkList {

    def ++[A](darkList1: DarkList[A], darkList2: DarkList[A]) : DarkList[A] = {
        
        darkList1 match {
            case DarkNil => darkList2
            case DarkCons(x, xs) => DarkCons(x, ++(xs, darkList2))
        }
    }

    def reverse[A](darkList: DarkList[A]) : DarkList[A] = {
        darkList match {
            case DarkNil => DarkNil
            case DarkCons(x, xs) => DarkList.++(DarkList.reverse(xs), DarkCons(x, DarkNil))
        }
    }

    def tail[A](darkList: DarkList[A]) : DarkList[A] = {
        darkList match {
            case DarkNil => DarkNil
            case DarkCons(x, xs) => xs
        }
    }

    def drop[A](darkList: DarkList[A], n: Int) : DarkList[A] = {
        darkList match {
            case DarkNil => DarkNil
            case DarkCons(x, xs) => drop(xs, n-1)
        }
    }

    def dropWhile[A](darkList: DarkList[A])(f: A => Boolean) : DarkList[A] = {
        darkList match {
            case DarkNil => DarkNil
            case DarkCons(x, xs) => {
                if (f(x)) DarkCons(x, xs)
                else dropWhile(xs)(f)
            }
        }
    }

    def setHead[A](darkList: DarkList[A], newHead: A) : DarkList[A] = {
        darkList match {
            case DarkNil => DarkCons(newHead, DarkNil)
            case DarkCons(x, xs) => DarkCons(newHead, xs)
        }
    }

    def dropLast[A](darkList: DarkList[A]) : DarkList[A] = {

        darkList match {
            case DarkNil => DarkNil
            case DarkCons(x, xs) => {
                DarkList.reverse(DarkList.tail(DarkList.reverse(darkList)))
            }
        }

    }

    def foldLeft[A, B](darkList: DarkList[A], z: B)(f: (A, B) => B) : B = {
        darkList match {
            case DarkNil => z
            case DarkCons(x, xs) => foldLeft(xs, f(x, z))(f)
        }
    }

    def foldRight[A, B](darkList: DarkList[A], z: B)(f: (A, B) => B) : B = {
    
        darkList match {
            case DarkNil => z
            case DarkCons(x, xs) => f(x, foldRight(xs, z)(f))
        }    
    }

    def map[A, B](darkList: DarkList[A])(f: A => B) : DarkList[B] = {

        darkList match {
            case DarkNil => DarkNil
            case DarkCons(x, xs) => DarkCons(f(x), map(xs)(f))
        }
    }


    def length[A](darkList: DarkList[A]) : Int = {
        DarkList.foldRight(darkList, 0)((x: A, y: Int) => y+1)
    }


    def sum(intList: DarkList[Int]) : Int = {
       intList match {
           case DarkNil => 0
           case DarkCons(x, xs) => x + sum(xs)
       }
    }

    def apply[A](as: A*) : DarkList[A] = {
        if (as.isEmpty) DarkNil
        else DarkCons(as.head, this.apply(as.tail: _*))
    }
}