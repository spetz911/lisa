import Lisp._

/**
 * Created by oleg on 06/04/14.
 */
object Parser {


  sealed abstract class LToken
  case object LeftParenthesis extends LToken
  case object RightParenthesis extends LToken
  case object LQuote extends LToken
  case object LUnquote extends LToken
  case class LInt(value: Int) extends LToken
  case class LDouble(value: Double) extends LToken
  case class LAtom(value: String) extends LToken


  def tokenize(ss: String): Array[LToken] = {
    val ss1 = ss.replace("(", " ( ").replace(")", " ) ").replace("`", " ` ").replace(",", " , ").replace("~", " ~ ")
    val tokens = Array(ss1) flatMap (_ split " ") flatMap (_ split "\n") filter (!_.isEmpty)
    for (ss <- tokens)
    yield ss match {
      case "(" => LeftParenthesis
      case ")" => RightParenthesis
      case "`" => LQuote
      case "," | "~" => LUnquote
      case x => toSomeInt(x).map(LInt).getOrElse(
        toSomeDouble(x).map(LDouble).getOrElse(
          LAtom(x)))
    }
  }


  def makeFullAST(tokens: List[LToken]): List[CodeTree] = {
    val (branch, rest) = makeAST(tokens, List())
    branch.value
  }


  def makeAST(tokens: List[LToken], state: List[CodeTree]): (ABranch, List[LToken]) = {
    if (tokens.isEmpty) {
      (ABranch(state.reverse), tokens)
    } else {
      tokens.head match {
        case LeftParenthesis =>
          val tt = makeAST(tokens.tail, List())
          makeAST(tt._2, tt._1 :: state)
        case LQuote => {
          val (ABranch(tt), rest) = makeAST(tokens.tail, List())
          val tmp = ABranch(state.reverse ::: ABranch(List(AString("quote"), tt.head)) :: tt.tail)
          (tmp, rest)
        }
        case LUnquote => {
          val (ABranch(tt), rest) = makeAST(tokens.tail, List())
          val tmp = ABranch(state.reverse ::: ABranch(List(AString("unquote"), tt.head)) :: tt.tail)
          (tmp, rest)
        }
        case LInt(x) => makeAST(tokens.tail, ANumber(x) :: state)
        case LDouble(x) => makeAST(tokens.tail, ADouble(x) :: state)
        case LAtom("#f") => makeAST(tokens.tail, AFalse :: state)
        case LAtom("null") => makeAST(tokens.tail, ANull :: state)
        case LAtom(x) => makeAST(tokens.tail, AString(x) :: state)
        case RightParenthesis => (ABranch(state.reverse), tokens.tail)
      }
    }
  }

}
