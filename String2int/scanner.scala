// 
// Description: A Scala implementation of a parser for integer literals
//
package scanner

import java.io.{Reader, PushbackReader, StringReader}
import scala.annotation.tailrec

// Token data
sealed abstract class Token {
    def literal: String
}
case class IntToken(literal: String, value: BigInt) extends Token {
    override def toString: String = s"IntToken($literal, $value)"
}
case class LongToken(literal: String, value: Long) extends Token {
    override def toString: String = s"LongToken($literal, $value)"
}
case class DecimalToken(literal: String, value: BigDecimal) extends Token {
    override def toString: String = s"DecimalToken($literal, $value)"
}
case class FloatToken(literal: String, value: Float) extends Token {
    override def toString: String = s"FloatToken($literal, $value)"
}
case class DoubleToken(literal: String, value: Double) extends Token {
    override def toString: String = s"DoubleToken($literal, $value)"
}
case class StringToken(literal: String) extends Token {
    override def toString: String = s"StringToken($literal)"
}
case class IdToken(literal: String) extends Token {
    override def toString: String = s"IdToken($literal)"
}
case class KeywordToken(literal: String) extends Token {
    override def toString: String = s"KeywordToken($literal)"
}
case class SymbolToken(literal: String) extends Token {
    override def toString: String = s"SymbolToken($literal)"
}
case class CommentToken(literal: String) extends Token {
    override def toString: String = s"CommentToken($literal)"
}
case class IllegalToken(literal: String) extends Token {
    override def toString: String = s"IllegalToken($literal)"
}
case object EOFToken extends Token {
    override def literal: String = "EOF"
    override def toString: String = "EOFToken"
}


class StateMachine(r: PushbackReader) extends Iterator[Token] {
    // constructors
    def this(r: Reader) = {
        this(new PushbackReader(r))
    }

    def this(input: String) = {
        this(new StringReader(input))
    }

    // The states of the parser
    enum State { case START, FINISH, ZERO,
                  DEC, OCT, B, BIN, X, HEX,
                  DOT, POINT, FRAC, E, EXP, EXP_SIGN,
                  HEXFLOAT, P, HEXEXP, HEXEXP_SIGN,
                  STR, STR_ESC, STR_END, STR_END_ESC,
                  ALPH, ALNUM,
                  SLASH, LINE_COMMENT, BLOCK_COMMENT, COMMENT_STAR,
                  SYM1, SYM_FOLLOW
    }
    import State._

    val EOF = -1
    var c: Int = 0
    var st  = State.START

    def hasNext: Boolean = {
        r.ready() && c != EOF
    }

    def next(): Token = {
        st = State.START
        var buf = new StringBuilder
        var acc: BigInt = 0

        @tailrec
        def forward(): Token = {
            try {
                if (r.ready()) {
                    c = r.read()
                } else {
                    c = EOF
                }
            } catch {
                case _: java.io.IOException => c = EOF
            }

            st match {
                case START if c == EOF => EOFToken
                case START if c == '0' =>
                    buf.append(c.toChar)
                    st = ZERO
                    forward()
                case START if '1' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    acc = c - '0'
                    st = DEC
                    forward()
                case START if c == '.' =>
                    buf.append(c.toChar)
                    st = DOT
                    forward()
                case START if 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' =>
                    buf.append(c.toChar)
                    st = ALPH
                    forward()
                case START if c == '"' =>
                    st = STR
                    forward()
                case START if c == '/' =>
                    st = SLASH
                    forward()
                case START if c == ' ' || c == '\t' || c == '\n' || c == '\r' =>
                    // skip whitespace
                    forward()
                case START =>
                    buf.append(c.toChar)
                    st = SYM1
                    forward()

                case ZERO if '0' <= c && c <= '7' =>
                    buf.append(c.toChar)
                    acc = 8 * acc + (c - '0')
                    st = OCT
                    forward()
                case ZERO if c == 'b' || c == 'B' =>
                    buf.append(c.toChar)
                    st = B
                    forward()
                case ZERO if c == 'x' || c == 'X' =>
                    buf.append(c.toChar)
                    st = X
                    forward()
                case ZERO if c == '.' =>
                    buf.append(c.toChar)
                    st = POINT
                    forward()
                case ZERO if c == 'L' || c == 'l' =>
                    // push back the character and finish
                    buf.append(c.toChar)
                    LongToken(buf.toString, 0L)
                case ZERO if c == 'D' || c == 'd' =>
                    // push back the character and finish
                    buf.append(c.toChar)
                    DoubleToken(buf.toString, 0.0)
                case ZERO if c == 'F' || c == 'f' =>
                    // push back the character and finish
                    buf.append(c.toChar)
                    FloatToken(buf.toString, 0.0f)
                case ZERO =>
                    // push back the character and finish
                    r.unread(c)
                    IntToken("0", BigInt(0))

                case DEC if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    acc = 10 * acc + (c - '0')
                    forward()
                case DEC if c == '.' =>
                    buf.append(c.toChar)
                    st = POINT
                    forward()
                case DEC if c == 'e' || c == 'E' =>
                    buf.append(c.toChar)
                    st = E
                    forward()
                case DEC if c == 'L' || c == 'l' =>
                    // push back the character and finish
                    buf.append(c.toChar)
                    LongToken(buf.toString, acc.toLong)
                case DEC if c == 'D' || c == 'd' =>
                    // push back the character and finish
                    buf.append(c.toChar)
                    DoubleToken(buf.toString, acc.toDouble)
                case DEC if c == 'F' || c == 'f' =>
                    // push back the character and finish
                    buf.append(c.toChar)
                    FloatToken(buf.toString, acc.toFloat)
                case DEC =>
                    // push back the character and finish
                    r.unread(c)
                    IntToken(buf.toString, acc) 

                case OCT if '0' <= c && c <= '7' =>
                    buf.append(c.toChar)
                    acc = 8 * acc + (c - '0')
                    forward()
                case OCT => 
                    // push back the character and finish
                    r.unread(c)
                    IntToken(buf.toString, acc)
                
                case B if c == '0' || c == '1' =>
                    buf.append(c.toChar)
                    acc = 2 * acc + (c - '0')
                    st = BIN
                    forward()
                case B =>
                    // push back the character and report error
                    r.unread(c)
                    IllegalToken(buf.toString)

                case BIN if c == '0' || c == '1' =>
                    buf.append(c.toChar)
                    acc = 2 * acc + (c - '0')
                    st = BIN
                    forward()
                case BIN if c == '_' =>
                    // ignore underscore
                    buf.append(c.toChar)
                    forward()
                case BIN =>
                    // push back the character and finish
                    r.unread(c)
                    IntToken(buf.toString, acc)

                case X if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    acc = 16 * acc + (c - '0')
                    st = HEX
                    forward()
                case X if 'A' <= c && c <= 'F' =>
                    buf.append(c.toChar)
                    acc = 16 * acc + (c - 'A' + 10)
                    st = HEX
                    forward()
                case X if 'a' <= c && c <= 'f' =>
                    buf.append(c.toChar)
                    acc = 16 * acc + (c - 'a' + 10)
                    st = HEX
                    forward()
                case X => 
                    // push back the character and report error
                    r.unread(c)
                    IllegalToken(buf.toString)

                case HEX if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    acc = 16 * acc + (c - '0')
                    st = HEX
                    forward()
                case HEX if 'A' <= c && c <= 'F' =>
                    buf.append(c.toChar)
                    acc = 16 * acc + (c - 'A' + 10)
                    st = HEX
                    forward()
                case HEX if 'a' <= c && c <= 'f' =>
                    buf.append(c.toChar)
                    acc = 16 * acc + (c - 'a' + 10)
                    st = HEX
                    forward()
                case HEX if c == '_' =>
                    // ignore underscore
                    buf.append(c.toChar)
                    forward()
                case HEX =>
                    // push back the character and finish
                    r.unread(c)
                    IntToken(buf.toString, acc)

                // .999 is a valid float
                case DOT if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = FRAC
                    forward()
                case DOT => 
                    r.unread(c)
                    SymbolToken(".")

                // the point of "999."
                case POINT if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = FRAC
                    forward()
                case POINT if c == 'e' || c == 'E' =>
                    buf.append(c.toChar)
                    st = E
                    forward()
                case POINT if c == 'd' || c == 'D' =>
                    buf.append(c.toChar)
                    val str = buf.toString
                    st = FINISH
                    DoubleToken(str, str.toDouble)
                case POINT if c == 'f' || c == 'F' =>
                    buf.append(c.toChar)
                    val str = buf.toString
                    st = FINISH
                    FloatToken(str, str.toFloat)
                case POINT => 
                    r.unread(c)
                    val digits = buf.toString
                    DecimalToken(digits, BigDecimal(digits))

                case FRAC if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    forward()
                case FRAC if c == 'e' || c == 'E' =>
                    buf.append(c.toChar)
                    st = E
                    forward()
                case FRAC if c == 'd' || c == 'D' =>
                    buf.append(c.toChar)
                    val str = buf.toString
                    st = FINISH
                    DoubleToken(str, str.toDouble)
                case FRAC if c == 'f' || c == 'F' =>
                    buf.append(c.toChar)
                    val str = buf.toString
                    st = FINISH
                    FloatToken(str, str.toFloat)
                case FRAC => 
                    // push back the character and finish
                    r.unread(c)
                    val digits = buf.toString
                    DecimalToken(digits, BigDecimal(digits))

                case E if c == '+' || c == '-' =>
                    buf.append(c.toChar)
                    st = EXP_SIGN
                    forward()
                case E if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = EXP
                    forward()
                case E => 
                    // push back the character and finish
                    r.unread(c)
                    IllegalToken(buf.toString)

                case EXP if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    forward()
                case EXP if c == 'd' || c == 'D' =>
                    buf.append(c.toChar)
                    val str = buf.toString
                    DoubleToken(str, str.toDouble)
                case EXP if c == 'f' || c == 'F' =>
                    buf.append(c.toChar)
                    val str = buf.toString
                    FloatToken(str, str.toFloat)
                case EXP => 
                    // push back the character and finish
                    r.unread(c)
                    DecimalToken(buf.toString, BigDecimal(buf.toString))
                case EXP_SIGN if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = EXP
                    forward()
                case EXP_SIGN =>
                    // push back the character and finish
                    r.unread(c)
                    IllegalToken(buf.toString)

                case SLASH if c == '/' =>
                    // single line comment
                    buf.append(c.toChar)
                    st = LINE_COMMENT
                    forward()
                case SLASH if c == '*' =>
                    // multi-line comment
                    buf.append(c.toChar)
                    st = BLOCK_COMMENT
                    forward()
                case SLASH if c == '=' =>
                    // "/=" assignment operator
                    SymbolToken("/=")
                case SLASH =>
                    // push back the character and finish
                    r.unread(c)
                    SymbolToken("/")

                case LINE_COMMENT if c == '\n' || c == EOF =>
                    CommentToken(buf.toString)
                case LINE_COMMENT =>
                    buf.append(c.toChar)
                    forward()
                case BLOCK_COMMENT if c == '*' =>
                    // detect "/* .... *"
                    buf.append(c.toChar)
                    st = COMMENT_STAR
                    forward()
                case BLOCK_COMMENT if c == EOF =>
                    // end of file in a block comment
                    IllegalToken(buf.toString)
                case BLOCK_COMMENT =>
                    // in "/* ... */" comment
                    buf.append(c.toChar)
                    forward()
                case COMMENT_STAR if c == '/' =>
                    // end of block comment "/* ... */"
                    buf.append(c.toChar)
                    CommentToken(buf.toString)
                case COMMENT_STAR if c == '*' =>
                    // detect "/* ... **"
                    buf.append(c.toChar)
                    forward()
                case COMMENT_STAR if c == EOF =>
                    // end of file in a block comment
                    IllegalToken(buf.toString)
                case COMMENT_STAR =>
                    // in "/* ... *_"
                    // continue the block comment
                    buf.append(c.toChar)
                    st = BLOCK_COMMENT
                    forward()

                case ALPH if '0' <= c && c <= '9' || 
                             'A' <= c && c <= 'Z' || 
                             'a' <= c && c <= 'z' || 
                             c == '_' =>
                    buf.append(c.toChar)
                    st = ALNUM
                    forward()
                case ALPH =>
                    // push back the character and finish
                    r.unread(c)
                    IdToken(buf.toString)
                case ALNUM if '0' <= c && c <= '9' || 
                              'A' <= c && c <= 'Z' || 
                              'a' <= c && c <= 'z' || 
                              c == '_' =>
                    buf.append(c.toChar)
                    forward()
                case ALNUM =>
                    // push back the character and finish
                    r.unread(c)
                    IdToken(buf.toString)
            }
        }

        forward()     // the return value of next()
    }
}
