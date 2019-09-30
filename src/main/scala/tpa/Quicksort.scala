package tpa

object Quicksort {

  def qsort[A <% Ordered[A]](a: Seq[A]): Seq[A] = {
    if (a.size <= 1) a
    else {
      val (p,a0) = a partition {x => x == a(0)}
      val (a1,a2) = a0 partition {x => x < a(0)}
      // printf("a1 => %s\n", a1)
      // printf("a2 => %s\n", a2)
      val b1 = qsort(a1)
      val b2 = qsort(a2)
      b1 ++ p ++ b2
    }
  }

  val a = Vector(
    74, 2, 30, 24, 32, 25, 54, 62, 98, 95, 27,
    73, 52, 12, 15, 24, 99, 37, 51, 19, 97)

}
