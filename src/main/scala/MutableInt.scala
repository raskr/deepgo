class MutableInt(var value: Int) {
  def +=(x: Int) = value += x
  def -=(x: Int) = value -= x
  def *=(x: Int) = value *= x
  def /=(x: Int) = value /= x
}
