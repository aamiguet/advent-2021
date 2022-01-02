package ch.aamiguet

package object math {
  def naturalSum(n: Int) = (n * (n + 1)) / 2

  def modUp(n: Int, m: Int) = {
    val x = n % m
    if (x == 0)
      m
    else
      x
  }

}
