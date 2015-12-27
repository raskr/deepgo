import scala.annotation.tailrec
import scala.collection.mutable

object ArgParse {

  case class Arg(pref: String, v: String)
  type Args = mutable.Set[Arg]

  @tailrec
  def apply(remain: List[String], results: Args = mutable.Set()): Args =
    remain match {
      case pref :: value :: rem =>
        if (pref.startsWith("-")) results.add(Arg(pref, value))
        apply(rem, results)
      case Nil => results
    }

}
