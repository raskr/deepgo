// see https://github.com/akiradeveloper/sgf-scala
import scala.util.parsing.combinator.RegexParsers

object SGF extends RegexParsers {

  case class Collection(a: List[GameTree])
  case class GameTree(a: Sequence, b: List[GameTree])
  case class Sequence(a: List[Node])
  case class Node(props: List[Property])

  case class Property(a: PropIdent, b: List[PropValue])
  case class PropIdent(a: String)
  case class PropValue(a: CValueType)

  trait CValueType
  trait ValueType extends CValueType
  case class Compose(a: ValueType, b: ValueType) extends CValueType

  // case object None extends ValueType
  case class Number(a: Int) extends ValueType
  case class Real(a: scala.Double) extends ValueType
  case class Double(a: Int) extends ValueType
  case class Color(a: Char) extends ValueType
  case class SimpleText(a: String) extends ValueType
  case class Text(a: String) extends ValueType
  case class Point(a: Char, b: Char) extends ValueType
  case object Unknown extends ValueType
  def int(x: Char): Int = if (x > 'z') x - 'A' + 1 else x -'a' + 1
  def char(x: Int): Char = ( if (x > 26) 'A'+x-1 else 'a'+x-1 ).toChar

  private def concat(xs: List[String]) = xs.fold(""){ (acc, e) => acc + e }

  def pAll = pCollection
  // Collection = List[GameTree]
  def pCollection = pGameTree.+ ^^ Collection
  def pGameTree: Parser[GameTree] = "(" ~ pSequence ~ pGameTree.* ~ ")" ^^ {
    case "(" ~ seq ~ gts ~ ")" => GameTree(seq, gts)
  }
  def pSequence = pNode.+ ^^ Sequence
  // moves
  def pNode = ";" ~> pProperty.* ^^ Node
  def pPropIdent = pUcLetter.+ ^^ { case xs => PropIdent(concat(xs)) }
  def pProperty =
    pPropIdent >> {
      case pid @ PropIdent(id) =>
        def propNone = "[" ~> """\s*""".r <~ "]" ^^^ { Property(pid, List()) }
        def prop1[T <: CValueType](p: Parser[T]) = "[" ~> p <~ "]" ^^ { case x => Property(pid, List(PropValue(x))) }
        def propList[T <: CValueType](p: Parser[T]) = ("[" ~> p <~ "]").+ ^^ { case xs => Property( pid, xs map PropValue ) }
        def propElist[T <: CValueType](p: Parser[T]) = propNone | propList(p)
        def pCompressedPoint = pCompose(pPoint, pPoint)
        def propListOfPoint = propList(pCompressedPoint | pPoint)
        def propElistOfPoint = propElist(pCompressedPoint | pPoint)
        def propUnknown = {
          def pSkip = """(\w)*""".r ^^^ { Unknown }
          propElist(pSkip) | prop1(pSkip)
        }

        id match {
          // Move
          case "B" => propNone | prop1(pPoint) // B[] means pass
          // case "KO" => pNone ^^ { case _ => Property(pid, List()) }
          // case "KO" => prop1(pNone) // List(None) or List()?
          case "KO" => propNone
          case "NM" => prop1(pNumber)
          case "W" => propNone | prop1(pPoint)

          // Setup
          case "AB" => propListOfPoint
          case "AE" => propListOfPoint
          case "AW" => propListOfPoint
          case "PL" => prop1(pColor)

          // node Annotation
          case "C" => prop1(pText)
          case "DM" => prop1(pDouble)
          case "GB" => prop1(pDouble)
          case "GW" => prop1(pDouble)
          case "HO" => prop1(pDouble)
          case "N" => prop1(pSimpleText)
          case "UC" => prop1(pDouble)
          case "V" => prop1(pReal)

          // Move Annotation
          case "BM" => prop1(pDouble)
          case "DO" => propNone
          case "IT" => propNone
          case "TE" => prop1(pDouble)

          // Markup
          case "AR" => propList(pCompose(pPoint, pPoint))
          case "CR" => propListOfPoint
          case "DD" => propElistOfPoint
          case "LB" => propList(pCompose(pPoint, pSimpleText))
          case "LN" => propList(pCompose(pPoint, pPoint))
          case "MA" => propListOfPoint
          case "SL" => propListOfPoint
          case "SQ" => propListOfPoint
          case "TR" => propListOfPoint

          // Root
          case "AP" => prop1(pCompose(pSimpleText1, pSimpleText))
          case "CA" => prop1(pSimpleText)
          case "FF" => prop1(pNumber) // 1-4
          case "GM" => prop1(pNumber) // 1-16
          case "ST" => prop1(pNumber) // 0-3
          case "SZ" => prop1(pNumber) | prop1(pCompose(pNumber, pNumber))

          // Game Info
          case "AN" => prop1(pSimpleText)
          case "BR" => prop1(pSimpleText)
          case "BT" => prop1(pSimpleText)
          case "CP" => prop1(pSimpleText)
          case "DT" => prop1(pSimpleText)
          case "EV" => prop1(pSimpleText)
          case "GN" => prop1(pSimpleText)
          case "GC" => prop1(pText)
          case "ON" => prop1(pSimpleText)
          case "OT" => prop1(pSimpleText)
          case "PB" => prop1(pSimpleText)
          case "PC" => prop1(pSimpleText)
          case "PW" => prop1(pSimpleText)
          case "RE" => prop1(pSimpleText)
          case "RO" => prop1(pSimpleText)
          case "RU" => prop1(pSimpleText)
          case "SO" => prop1(pSimpleText)
          case "TM" => prop1(pReal)
          case "US" => prop1(pSimpleText)
          case "WR" => prop1(pSimpleText)
          case "WT" => prop1(pSimpleText)

          // Timing
          case "BL" => prop1(pReal)
          case "OB" => prop1(pNumber)
          case "OW" => prop1(pNumber)
          case "WL" => prop1(pReal)

          // Misc
          case "FG" => propNone | prop1(pCompose(pNumber, pSimpleText))
          case "PM" => prop1(pNumber)
          case "VW" => propElistOfPoint

          // Go specific
          case "HA" => prop1(pNumber)
          case "KM" => prop1(pReal)
          case "TB" => propElistOfPoint
          case "TW" => propElistOfPoint

          // Unknown
          case _  => propUnknown
        }
    }

  private def pCompose[A <: ValueType, B <: ValueType](pa: Parser[A], pb: Parser[B]) =
    pa ~ ":" ~ pb ^^ { case x ~ ":" ~ y => Compose(x, y) }

  // def pNone = "" ^^ { _ => None }
  private def pUcLetter = """[A-Z]""".r
  private def pDigit = """[0-9]""".r
  private def pNumber_ = ("+"|"-"|"") ~ rep1(pDigit) ^^ {
    case sig ~ digits =>
      val a: Int = sig match {
        case "+" => 1
        case "-" => -1
        case "" => 1
      }
      val b: Int = concat(digits).toInt
      a * b
  }
  private def pNumber = pNumber_ ^^ Number
  private def pReal = pNumber_ ~ "." ~ rep1(pDigit) ^^ {
    case int ~ "." ~ decimal =>
      val a = int
      val b = ("0." + concat(decimal)).toDouble
      Real(if (a >= 0) a + b else a - b)
  } | pNumber ^^ { case x =>
    val Number(n) = x
    Real(n.toDouble)
  }
  private def pDouble = ("1"|"2") ^^ { case x => Double(x.toInt) }
  private def pColor = ("B"|"W") ^^ { case x => Color(x.charAt(0)) }
  private def pSimpleText1 = """[^:]+""".r ^^ SimpleText
  private def pSimpleText = """[^]]+""".r ^^ SimpleText
  private def pText = """(\\]|[^]])+""".r ^^ Text // FIXME
  private def pPoint = repN(2, """([a-z]|[A-Z])""".r) ^^ { case List(a, b) => Point(a.head, b.head) }

}
