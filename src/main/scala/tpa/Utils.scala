package tpa

import scala.util.Random

object Utils {

  /** Função que troca o conteúdo de duas posições, `i` e `j`, do vetor `a`.
    */
  def swap[A](a: Array[A], i: Int, j: Int) {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }


  def randomize[A](a: Array[A]): Array[A] = {
    for (i <- 0 to a.size-1) {
      val j = Random.nextInt(a.size)
      swap(a, i, j)
    }
    a
  }

  def randIntArray(n: Int, low: Int, high: Int): Array[Int] = {
    val a = Array.ofDim[Int](n)
    val gap = high - low
    for (i <- 0 until n) {
      a(i) = Random.nextInt(gap) + low
    }
    a
  }

  def randIntArray(n: Int, high: Int): Array[Int] = randIntArray(n, 0, high)

  def randIntArray(n: Int): Array[Int] = randIntArray(n, 0, 2*n)

  def randUniqueIntArray(n: Int, low: Int, high: Int): Array[Int] = {
    var a = Set.empty[Int]
    val gap = high - low
    if (gap < n) {
      throw new IllegalArgumentException("Not enought unique values.")
    }
    while (a.size < n) {
      a = a + Random.nextInt(gap) + low
    }
    randomize(a.toArray)
  }

  def randUniqueIntArray(n: Int, high: Int): Array[Int] = randUniqueIntArray(n, 0, high)

  def randUniqueIntArray(n: Int): Array[Int] = randUniqueIntArray(n, 0, 2*n)

}
