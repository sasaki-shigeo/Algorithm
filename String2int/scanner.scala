// 
// Description: A Scala implementation of a parser for integer literals
//
package scanner

import java.io.{Reader, PushbackReader, StringReader}
import scala.annotation.tailrec

var alphabets = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
var punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~"

def isAlpha(c: Int): Boolean = alphabets.indexOf(c.toChar) >= 0
def isDigit(c: Int): Boolean = '0' <= c && c <= '9'
def isAlnum(c: Int): Boolean = isAlpha(c) || isDigit(c) || c == '$'
def isPunct(c: Int): Boolean = punctuation.indexOf(c.toChar) >= 0

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

// Utity functions
def str(str: String) = str
def str(buf: StringBuilder): String = buf.toString
def removeUnderscores(str: String): String = {
    str.filter(_ != '_')
}
def removeUnderscores(buf: StringBuilder): String = {
    buf.filter(_ != '_').toString
}

// Lexical analyzer state machine

class StateMachine(r: PushbackReader) extends Iterator[Token] {
    // constructors
    def this(r: Reader) = {
        this(new PushbackReader(r))
    }

    def this(input: String) = {
        this(new StringReader(input))
    }

    // The states of the parser
    enum State { case START, SYMBOL,
                  ZERO, DEC, DEC_SEP, OCT, OCT_SEP, B, BIN, X, HEX,
                  DOT, POINT, FRAC, FRAC_SEP, E, EXP, EXP_SEP, EXP_SIGN,
                  HEXFLOAT, HEXFLOAT_SEP, HEXPOINT,
                  P, HEXEXP, HEXEXP_SEP, HEXEXP_SIGN,
                  // String parser not implemented yet
                  // STR, STR_ESC, STR_END, STR_END_ESC,
                  ALPH, ALNUM,
                  SLASH, LINE_COMMENT, BLOCK_COMMENT, COMMENT_STAR
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
                case START if 'A' <= c && c <= 'Z' ||
                              'a' <= c && c <= 'z' ||
                              c == '_' =>
                    buf.append(c.toChar)
                    st = ALPH
                    forward()
                case START if c == '/' =>
                    buf.append(c.toChar)
                    st = SLASH
                    forward()
                case START if c == ' ' || c == '\t' || c == '\n' || c == '\r' =>
                    // skip whitespace
                    forward()
                case START =>
                    buf.append(c.toChar)
                    st = SYMBOL
                    forward()

                case ZERO if '0' <= c && c <= '7' =>
                    buf.append(c.toChar)
                    acc = 8 * acc + (c - '0')
                    st = OCT
                    forward()
                case ZERO if c == '_' =>
                    buf.append(c.toChar)
                    st = OCT_SEP
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
                    buf.append(c.toChar)
                    LongToken(buf.toString, 0L)
                case ZERO if c == 'D' || c == 'd' =>
                    buf.append(c.toChar)
                    DoubleToken(buf.toString, 0.0)
                case ZERO if c == 'F' || c == 'f' =>
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
                case DEC if c == '_' =>
                    buf.append(c.toChar)
                    st = DEC_SEP
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
                    buf.append(c.toChar)
                    LongToken(buf.toString, acc.toLong)
                case DEC if c == 'D' || c == 'd' =>
                    buf.append(c.toChar)
                    DoubleToken(buf.toString, acc.toDouble)
                case DEC if c == 'F' || c == 'f' =>
                    buf.append(c.toChar)
                    FloatToken(buf.toString, acc.toFloat)
                case DEC =>
                    // push back the character and finish
                    r.unread(c)
                    IntToken(buf.toString, acc)

                case DEC_SEP if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    acc = 10 * acc + (c - '0')
                    st = DEC
                    forward()
                case DEC_SEP => 
                    // push back the character and finish
                    r.unread(c)
                    IllegalToken(buf.toString)

                case OCT if '0' <= c && c <= '7' =>
                    buf.append(c.toChar)
                    acc = 8 * acc + (c - '0')
                    forward()
                case OCT if c == '_' =>
                    buf.append(c.toChar)
                    st = OCT_SEP
                    forward()
                case OCT if c == 'L' || c == 'l' =>
                    buf.append(c.toChar)
                    LongToken(buf.toString, acc.toLong)
                case OCT => 
                    // push back the character and finish
                    r.unread(c)
                    IntToken(buf.toString, acc)

                case OCT_SEP if '0' <= c && c <= '7' =>
                    buf.append(c.toChar)
                    acc = 8 * acc + (c - '0')
                    st = OCT
                    forward()
                case OCT_SEP =>
                    // push back the character and finish
                    r.unread(c)
                    IllegalToken(buf.toString)
                
                // "0b", "0B" or digit seperator "_"
                // generally "0[bB]([01]+_)*" in regex
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
                    buf.append(c.toChar)
                    st = B
                    forward()
                case BIN =>
                    // push back the character and finish
                    r.unread(c)
                    IntToken(buf.toString, acc)

                // "0x", "0X" or digit seperator "_"
                // generally "0[xX]([0-9a-fA-F]+_)*" in regex
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
                case X if c == '.' =>
                    buf.append(c.toChar)
                    st = HEXPOINT
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
                    buf.append(c.toChar)
                    st = X
                    forward()
                case HEX if c == 'L' || c == 'l' =>
                    buf.append(c.toChar)
                    LongToken(buf.toString, acc.toLong)
                case HEX if c == '.' =>
                    buf.append(c.toChar)
                    st = HEXFLOAT
                    forward()
                case HEX if c == 'p' || c == 'P' =>
                    buf.append(c.toChar)
                    st = P
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
                // this parser does not recognize ".." operator
                case DOT =>
                    r.unread(c)
                    SymbolToken(".")

                // Parsing floating point numbers
                //
                // Note1:
                // The variable acc is used only for integer literals.
                // This code delegates the floating point number parsing
                // to the String.{toFloat, toDouble, toBigDecimal} methods
                // because of the complexity.
                //
                // Note2:
                // String.{toFloat, toDouble, toBigDecimal} 
                // (and also String.{toInt, toLong, toBigInt})
                // call java.lang.*.parse* methods;
                // but they do not accept underscores in the input strings.
                // So, we need to remove underscores
                // before calling these methods.

                // reading the point on "999."

                // at the case of "999.9"
                case POINT if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = FRAC
                    forward()
                // at the case of "999.E", exp part is necessary
                case POINT if c == 'e' || c == 'E' =>
                    buf.append(c.toChar)
                    st = E
                    forward()
                // at the case of "999.d" or "999.D", double literal
                case POINT if c == 'd' || c == 'D' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    val dbl = digits.filter(_ != '_').toDouble
                    // Double#toDouble calls java.lang.Double.parseDouble
                    // which cannot accept underscores
                    DoubleToken(digits, dbl)
                // at the case of "999.f" or "999.F", float literal
                case POINT if c == 'f' || c == 'F' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    val flt = digits.filter(_ != '_').toFloat
                    FloatToken(digits, flt)
                // at the case of "999.", decimal literal
                case POINT =>
                    r.unread(c)
                    val digits = buf.toString
                    val decimal = BigDecimal(digits.filter(_ != '_'))
                    DecimalToken(digits, decimal)

                case FRAC if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    forward()
                case FRAC if c == '_' =>
                    buf.append(c.toChar)
                    st = FRAC_SEP
                    forward()
                // reading ".999e", exp part is necessary
                case FRAC if c == 'e' || c == 'E' =>
                    buf.append(c.toChar)
                    st = E
                    forward()
                // ".999d" is a double literal
                case FRAC if c == 'd' || c == 'D' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    val dbl = digits.filter(_ != '_').toDouble
                    DoubleToken(digits, dbl)
                // ".999f" is a float literal
                case FRAC if c == 'f' || c == 'F' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    val flt = digits.filter(_ != '_').toFloat
                    FloatToken(digits, flt)
                case FRAC =>
                    // push back the character and finish
                    r.unread(c)
                    val digits = buf.toString
                    val decimal = BigDecimal(digits.filter(_ != '_'))
                    DecimalToken(digits, decimal)

                case FRAC_SEP if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = FRAC
                    forward()
                case FRAC_SEP =>
                    // push back the character and finish
                    r.unread(c)
                    IllegalToken(buf.toString)

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
                case EXP if c == '_' =>
                    buf.append(c.toChar)
                    st = EXP_SEP
                    forward()
                case EXP if c == 'd' || c == 'D' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    DoubleToken(digits, digits.toDouble)
                case EXP if c == 'f' || c == 'F' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    FloatToken(digits, digits.toFloat)
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
                case EXP_SEP if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = EXP
                    forward()
                case EXP_SEP =>
                    // push back the character and finish
                    r.unread(c)
                    IllegalToken(buf.toString)

                // "0xFF.", "0xFF.F", "0x.FF" ans so on
                case HEXFLOAT if c == 'p' || c == 'P' =>
                    buf.append(c.toChar)
                    st = P
                    forward()
                case HEXFLOAT if '0' <= c && c <= '9' ||
                                 'A' <= c && c <= 'F' ||
                                 'a' <= c && c <= 'f'    =>
                    buf.append(c.toChar)
                    forward()
                case HEXFLOAT if c == '_' =>
                    buf.append(c)
                    st = HEXFLOAT_SEP
                    forward()
                // hexadecimal floats without 'P' are illegal
                case HEXFLOAT =>
                    r.unread(c)
                    IllegalToken(buf.toString)
                case HEXFLOAT_SEP if '0' <= c && c <= '9' ||
                                     'A' <= c && c <= 'F' ||
                                     'a' <= c && c <= 'f'    =>
                    buf.append(c.toChar)
                    st = HEXFLOAT
                    forward()
                case HEXFLOAT_SEP =>
                    r.unread(c)
                    IllegalToken(buf.toString)
                
                // reading "0x.", necessary to read [0-9A-Fa-f] digits
                case HEXPOINT if '0' <= c && c <= '9' ||
                                 'A' <= c && c <= 'F' ||
                                 'a' <= c && c <= 'f'    =>
                    buf.append(c.toChar)
                    st = HEXFLOAT
                    forward()
                case HEXPOINT => 
                    // push back the character and finish
                    r.unread(c)
                    IllegalToken(buf.toString)

                // "0xf.0p"; "[-+0-9]" needed
                case P if c == '+' || c == '-' =>
                    buf.append(c.toChar)
                    st = HEXEXP_SIGN
                    forward()
                case P if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = HEXEXP
                    forward()
                case P =>
                    r.unread(c)
                    IllegalToken(buf.toString)

                case HEXEXP_SIGN if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = HEXEXP
                    forward()
                case HEXEXP_SIGN =>
                    r.unread(c)
                    IllegalToken(buf.toString)

                case HEXEXP if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    forward()
                case HEXEXP if c == 'd' || c == 'D' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    val dbl = digits.filter(_ != '_').toDouble
                    DoubleToken(digits, dbl)
                case HEXEXP if c == 'f' || c == 'F' =>
                    buf.append(c.toChar)
                    val digits = buf.toString
                    val flt = digits.filter(_ != '_').toFloat
                    FloatToken(digits, flt)
                case HEXEXP if c == '_' =>
                    buf.append(c.toChar)
                    st = HEXEXP_SEP
                    forward()
                case HEXEXP =>
                    r.unread(c)
                    val digits = buf.toString
                    val dbl = digits.filter(_ != '_').toDouble
                    DoubleToken(digits, dbl)

                case HEXEXP_SEP if '0' <= c && c <= '9' =>
                    buf.append(c.toChar)
                    st = HEXEXP
                    forward()
                case HEXEXP_SEP =>
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

                case SYMBOL =>
                    if (! punctSet.contains(buf.toString)) {
                        r.unread(c)
                        IllegalToken(buf.toString)
                    }
                    else {
                        buf.append(c.toChar)
                        while (punctSet.contains(buf.toString)) {
                            c = r.read()
                            buf.append(c.toChar)
                        }
                        r.unread(c)
                        SymbolToken(buf.substring(0, buf.length - 1))
                    }
            }
        }

        forward()     // the return value of next()
    }
}

import scala.collection.mutable.HashSet

val punctSet: HashSet[String] = HashSet(
    "!", "!=", "%", "%=", "&", "&&", "&=", "&&=", "(", ")",
    "*", "*=", ",", "+", "+=", "++", "++-", "-", "-=", "--", "/", "/=",
    ":", ";", "<", "<=", "<<", "<<=",
    "=", "==", ">", ">=", ">>", ">>=", ">>>", ">>>=",
    "?", "@", "[", "]", "^", "^=",
    "{", "|", "|=", "||", "||=", "}", "~", "~="
)

import scala.collection.immutable.IntMap
val Empty = IntMap.empty[TrieNode]

class Trie(var children: IntMap[TrieNode]) {
    def get(key: CharSequence): Option[String] = {
        @tailrec
        def traverse(ix: Int, table: IntMap[TrieNode]): Option[String] = {
            if (ix >= key.length) {
                None
            }
            else {
                val c = key.charAt(ix)
                table.get(c) match {
                    case None => None
                    case Some(TrieNode(value, _)) if ix == key.length - 1 => value
                    case Some(TrieNode(_, Empty)) => None
                    case Some(TrieNode(_, children)) => traverse(ix + 1, children)
                }
            }
        }
        traverse(0, children)
    }
}

case class TrieNode(value: Option[String], children: IntMap[TrieNode])

val punctuationTable: Trie = new Trie(
    IntMap('('.toInt -> TrieNode(Some("("), Empty),
           ')'.toInt -> TrieNode(Some(")"), Empty),
           ','.toInt -> TrieNode(Some(","), Empty),
           ';'.toInt -> TrieNode(Some(";"), Empty)
    )
)
