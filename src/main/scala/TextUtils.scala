object TextUtils {

  def isEmpty(text: String) = text == null || text.length == 0
  def isNotEmpty(text: String) = !isEmpty(text)

}
