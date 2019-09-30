package tpa

import scala.util.Random.nextInt
import tpa.Utils.swap


object Sorting {

  /** Função que testa se o vetor `a` está ordenado.
    */
  def isSorted[A <% Ordered[A]](a: Array[A]): Boolean =
    (0 until a.size-1) forall {i => a(i) <= a(i+1)}

  /** Retorna a posição do menor valor contido no vetor `a`.
    */
  def argmin[A <% Ordered[A]](a: Array[A]): Int = argmin(a, 0, a.size - 1)

  /** Retorna a posição do menor valor contido no vetor `a`, à partir da posição `ini`.
    */
  def argmin[A <% Ordered[A]](a: Array[A], ini: Int): Int = argmin(a, ini, a.size - 1)

  /** Retorna a posição do menor valor contido no vetor `a`, entre os índices
    * `ini` e `fin`.
    */
  def argmin[A <% Ordered[A]](a: Array[A], ini: Int, fin: Int): Int = {
    var p = ini
    for (i <- (ini+1) to fin) {
      if (a(i) < a(p)) p = i
    }
    p
  }


  /** Retorna o menor valor contido no vetor `a`.
    */
  def minimum[A <% Ordered[A]](a: Array[A]): A = minimum(a, 0, a.size - 1)

  /** Retorna o menor valor contido no vetor `a`, à partir da posição `ini`.
    */
  def minimum[A <% Ordered[A]](a: Array[A], ini: Int): A = minimum(a, ini, a.size - 1)

    /** Retorna o menor valor contido no vetor `a`, entre os índices `ini` e `fin`.
    */
  def minimum[A <% Ordered[A]](a: Array[A], ini: Int, fin: Int): A = {
    var m = a(ini)
    for (i <- (ini+1) to fin) {
      if (a(i) < m) m = a(i)
    }
    m
  }


  /** Retorna a posição do maior valor contido no vetor `a`.
    */
  def argmax[A <% Ordered[A]](a: Array[A]): Int = argmax(a, 0, a.size - 1)

  /** Retorna a posição do maior valor contido no vetor `a`, à partir da posição `ini`.
    */
  def argmax[A <% Ordered[A]](a: Array[A], ini: Int): Int = argmax(a, ini, a.size - 1)

  /** Retorna a posição do maior valor contido no vetor `a`, entre os índices
    * `ini` e `fin`.
    */
  def argmax[A <% Ordered[A]](a: Array[A], ini: Int, fin: Int): Int = {
    var p = ini
    for (i <- (ini+1) to fin) {
      if (a(i) > a(p)) p = i
    }
    p
  }


  /** Retorna o maior valor contido no vetor `a`.
    */
  def maximum[A <% Ordered[A]](a: Array[A]): A = maximum(a, 0, a.size - 1)

  /** Retorna o maior valor contido no vetor `a`, à partir da posição `ini`.
    */
  def maximum[A <% Ordered[A]](a: Array[A], ini: Int): A = maximum(a, ini, a.size - 1)

  /** Retorna o maior valor contido no vetor `a`, entre os índices `ini` e `fin`.
    */
  def maximum[A <% Ordered[A]](a: Array[A], ini: Int, fin: Int): A = {
    var m = a(ini)
    for (i <- (ini+1) to fin) {
      if (a(i) > m) m = a(i)
    }
    m
  }


  /** Função que ordena um vetor `a` usando o algoritmo de ordenação por inserção
    * (insertion sort).
    */
  def insertSort[A <% Ordered[A]](a: Array[A]): Array[A] = insertSort(a, 0, a.size - 1)

  /** Função que ordena um vetor `a` usando o algoritmo de ordenação por inserção
    * (insertion sort), dentro de certos limites determinados de início e final
    * do vetor.
    */
  def insertSort[A <% Ordered[A]](a: Array[A], ini: Int, fin: Int): Array[A] = {
    var i = ini + 1
    while (i <= fin) {
      var j = i - 1
      while (j >= ini) {
        if (a(j) > a(j+1)) {
          swap(a, j, j+1)
        }
        j -= 1
      }
      i += 1
    }
    a
  }


  /** Função que ordena um vetor `a` usando o algoritmo de ordenação por seleção (select sort).
    */
  def selectSort[A <% Ordered[A]](a: Array[A]): Array[A] = selectSort(a, 0, a.size-1)

  /** Função que ordena um vetor `a` usando o algoritmo de ordenação por seleção
    * (select sort). A ordenação é feita à partir de um índice inicial indicado,
    * até o final do vetor.
    */
  def selectSort[A <% Ordered[A]](a: Array[A], ini: Int): Array[A] = selectSort(a, ini, a.size-1)

  /** Função que ordena um vetor `a` usando o algoritmo de ordenação por seleção
    * (select sort). A ordenação é feita dentro de limites inicial e final
    * indicados.
    */
  def selectSort[A <% Ordered[A]](a: Array[A], ini: Int, fin: Int): Array[A] = {
    for (i <- ini to (fin - 1)) {
      val j = argmin(a, i)
      swap(a, i, j)
    }
    a
  }


  /** Função que realiza a partição do algoritmo Quicksort. O elemento escolhido
    * como pivô sempre será o elemento da posição `r`, i.e., o último elemento
    * da subsequência que será particionada. Ao final da partição será retornado
    * um índice `q` tal que para todo elemento `a(i)`, p <= i <= q, `a(i) <=
    * pivo`; e para todo elemento `a(j)`, q < j <= r, `a(j) > pivo`. Esta função
    * implementa a versão do procedimento `partition` apresentada no livro
    * “Introduction to Algorithms”, 3rd edition.
    */
  def partition[A <% Ordered[A]](a: Array[A], p: Int, r: Int): Int = {
    val pivot = a(r)
    var i = p - 1
    for (j <- p to r-1) {
      if (a(j) <= pivot) {
        i = i + 1
        swap(a, i, j)
      }
    }
    swap(a, i+1, r)
    i+1
  }


  def quicksort[A <% Ordered[A]](a: Array[A]): Array[A] = quicksort(a, 0, a.size-1)

  def quicksort[A <% Ordered[A]](a: Array[A], p: Int, r: Int): Array[A] = {
    if (p < r) {
      val q = partition(a, p, r)
      quicksort(a, p, q-1)
      quicksort(a, q+1, r)
    }
    a
  }


  def randPartition[A <% Ordered[A]](a: Array[A], p: Int, r: Int): Int = {
    val w = r - p + 1
    val i = if (w > 0) nextInt(w) + p else p
    swap(a, i, r)
    partition(a, p, r)
  }

  def randQuicksort[A <% Ordered[A]](a: Array[A]): Array[A] = randQuicksort(a, 0, a.size-1)

  def randQuicksort[A <% Ordered[A]](a: Array[A], p: Int, r: Int): Array[A] = {
    if (p < r) {
      val q = randPartition(a, p, r)
      randQuicksort(a, p, q-1)
      randQuicksort(a, q+1, r)
    }
    a
  }


  def quickselect[A <% Ordered[A]](a: Array[A], k: Int): A = quickselect(a, k, 0, a.size-1)

  def quickselect[A <% Ordered[A]](a: Array[A], k: Int, p: Int, r: Int): A = {
    if (p >= r) a(p)
    else {
      val q = partition(a, p, r)
      if (k == q) a(q)
      else if (k < q) quickselect(a, k, p, q-1)
      else quickselect(a, k, q+1, r)
    }
  }

}
