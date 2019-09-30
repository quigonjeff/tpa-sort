package tpa

object InsertSort {

  /** Função que ordena um vetor `a` usando o algoritmo de ordenação por inserção
    * (insertion sort).
    */
  def apply[A <% Ordered[A]](a: Array[A]): Array[A] = apply(a, 0, a.size - 1)

  /** Função que ordena um vetor `a` usando o algoritmo de ordenação por inserção
    * (insertion sort), dentro de certos limites determinados de início e final
    * do vetor.
    */
  def apply[A <% Ordered[A]](a: Array[A], ini: Int, fin: Int): Array[A] = {
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

  /** Função que troca o conteúdo de duas posições, `i` e `j`, do vetor `a`.
    */
  def swap[A](a: Array[A], i: Int, j: Int) {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

}
