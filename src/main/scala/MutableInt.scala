class MutableInt(var value: Int) {
  def +=(x: Int) = value += x

  // three below are not used
  def -=(x: Int) = value -= x
  def *=(x: Int) = value *= x
  def /=(x: Int) = value /= x
}
