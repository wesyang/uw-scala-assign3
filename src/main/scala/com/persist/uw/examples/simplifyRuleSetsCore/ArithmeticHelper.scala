package com.persist.uw.examples.simplifyRuleSetsCore

import scala.annotation.tailrec

/**
  * Created by wyang on 11/4/2016.
  */
object ArithmeticHelper {

  def getPrimes(max: Int): List[Int] = {
    @tailrec def isPrime(i: Int, l: List[Int]): Boolean = {
      l match {
        case head +: rest =>
          if (head == 1) isPrime(i, rest)
          else if (head >= i) true
          else {
            if (i % head == 0) false
            else isPrime(i, rest)
          }
        case _ => true
      }
    }

    @tailrec def calculate(start: Int, max: Int, list: List[Int]): List[Int] = {
      if (start > max) list
      else {
        if (isPrime(start, list)) calculate(start + 1, max, list :+ start)
        else calculate(start + 1, max, list)
      }
    }

    val primes = List[Int](1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
    if (max < 32) primes
    else calculate(32, max, primes)
  }

  def absoluteValue (a :Int): Int=
  {
    if (a >= 0) a else -a;
  }

  def getCommonDenominator(a: Int, b: Int): Int = {

    @tailrec def calculateMore(small: Int, large: Int, primes: List[Int], cd: Int): Int = {
      primes match {
        case head +: rest =>
          if (head > small) cd
          else {
            if ( ((small % head) == 0) && ((large % head) == 0) )
              {
                calculateMore(small, large, rest, cd * head)
              }
            else calculateMore(small, large, rest, cd)
          }

        case _ => cd
      }
    }

    def  calculate (small:Int, large:Int) : Int = {
      if (small == 0) throw new IllegalArgumentException ("can not find denominator from zero")

      if (large % small == 0) small
      else
        {
          calculateMore (small, large, getPrimes(small / 2), 1)
        }
    }

    val left = absoluteValue (a)
    val right =absoluteValue (b)

    if (left < right) calculate (left, right) else calculate (right, left)
  }

}
