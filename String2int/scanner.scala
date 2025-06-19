// 
// Description: A Scala implementation of a parser for integer literals
//
//package scanner

import java.io.{Reader, PushbackReader, StringReader}
import scala.annotation.tailrec
import scala.util.control.TailCalls._

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


class Tokenizer(r: PushbackReader) extends Iterator[Token] {
    // constructors
    def this(r: Reader) = {
        this(new PushbackReader(r))
    }

    def this(input: String) = {
        this(new StringReader(input))
    }

    val EOF = -1
    var isEOF: Boolean = false

    // あとで inline にする
    // inline メソッドは，おそらく private
    private def canGetc: Boolean = {
        try {
            r.ready()
        } catch {
            case _: java.io.IOException => false
        }
    }
    private def getc(): Int = {
        try {
            val c = r.read()
            if (c == EOF) {
                isEOF = true
            }
            c
        } catch {
            case _: java.io.IOException => EOF
        }
    }
    private def ungetc(c: Int): Unit = {
        r.unread(c)
    }

    def hasNext: Boolean = (! isEOF)

    def next(): Token = {
        var buf = new StringBuilder
        var acc: BigInt = 0

        inline def putc(c: Int): Unit = {
            buf.append(c.toChar)
        }

        def start(): TailRec[Token] = getc() match {
            case EOF => done(EOFToken)
            case '0' =>
                putc('0')
                tailcall(zero())
            case c if '1' <= c && c <= '9' =>
                putc(c)
                acc = c - '0'
                tailcall(decimalInt())
            case '.' =>
                putc('.')
                tailcall(dot())
            case c if ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_' =>
                putc(c)
                tailcall(alpha())
            case '/' =>
                putc('/')
                tailcall(slash())
            case ' ' | '\t' | '\n' | '\r' =>
                // skip whitespaces
                tailcall(start())
            case '!' => getc() match {
                case '=' =>
                    done(SymbolToken("!="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("!"))
            }
            case '%' => getc() match {
                case '=' =>
                    done(SymbolToken("%="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("%"))
            }                
            case '&' => getc() match {
                case '&' => getc() match {
                    case '=' =>
                        done(SymbolToken("&&="))
                    case c =>
                        ungetc(c)
                        done(SymbolToken("&&"))
                }
                case '=' =>
                    done(SymbolToken("&="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("&"))
            }
            case '(' =>
                done(SymbolToken("("))
            case ')' =>
                done(SymbolToken(")"))
            case '*' => getc() match {
                case '=' =>
                    done(SymbolToken("*="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("*"))
            }
            case '+' => getc() match {
                case '+' => getc() match {
                    case '=' =>
                        done(SymbolToken("++="))
                    case c =>
                        ungetc(c)
                        done(SymbolToken("++"))
                }
                case '=' =>
                    done(SymbolToken("+="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("+"))
            }
            case ',' =>
                done(SymbolToken(","))
            case '-' => getc() match {
                case '-' => getc() match {
                    case '=' =>
                        done(SymbolToken("--="))
                    case c =>
                        ungetc(c)
                        done(SymbolToken("--"))
                }
                case '=' =>
                    done(SymbolToken("-="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("-"))
            }
            case ':' =>
                done(SymbolToken(":"))
            case ';' =>
                done(SymbolToken(";"))
            case '<' => getc() match {
                case '=' =>
                    done(SymbolToken("<="))
                case '<' => getc() match {
                    case '=' =>
                        done(SymbolToken("<<="))
                    case c =>
                        ungetc(c)
                        done(SymbolToken("<<"))
                }
                case c =>
                    ungetc(c)
                    done(SymbolToken("<"))
            }
            case '=' => getc() match {
                case '=' =>
                    done(SymbolToken("=="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("="))
            }
            case '>' => getc() match {
                case '=' =>
                    done(SymbolToken(">="))
                case '>' => getc() match {
                    case '=' =>
                        done(SymbolToken(">>="))
                    case '>' => getc() match {
                        case '=' =>
                            done(SymbolToken(">>>="))
                        case c =>
                            ungetc(c)
                            done(SymbolToken(">>>"))
                    }
                    case c =>
                        ungetc(c)
                        done(SymbolToken(">>"))
                }
                case c =>
                    ungetc(c)
                    done(SymbolToken(">"))
            }
            case '?' =>
                done(SymbolToken("?"))
            case '[' =>
                done(SymbolToken("["))
            case ']' =>
                done(SymbolToken("]"))
            case '^' => getc() match {
                case '=' =>
                    done(SymbolToken("^="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("^"))
            }
            case '{' =>
                done(SymbolToken("{"))
            case '|' => getc() match {
                case '|' => getc() match {
                    case '=' =>
                        done(SymbolToken("||="))
                    case c =>
                        ungetc(c)
                        done(SymbolToken("||"))
                }
                case '=' =>
                    done(SymbolToken("|="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("|"))
            }
            case '}' =>
                done(SymbolToken("}"))
            case '~' => getc() match {
                case '=' =>
                    done(SymbolToken("~="))
                case c =>
                    ungetc(c)
                    done(SymbolToken("~"))
            }
            case c =>
                done(IllegalToken("" + c.toChar))
        }

        def zero(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '7' =>
                putc(c)
                acc = 8 * acc + (c - '0')
                tailcall(octalInt())
            case '_' =>
                putc('_')
                tailcall(octalSep())
            case c if c == 'b' || c == 'B' =>
                putc(c)
                tailcall(zeroB())
            case c if c == 'x' || c == 'X' =>
                putc(c)
                tailcall(zeroX())
            case '.' =>
                putc('.')
                tailcall(point())
            case c if c == 'L' || c == 'l' =>
                putc(c)
                done(LongToken(buf.toString, 0L))
            case c if c == 'D' || c == 'd' =>
                putc(c)
                done(DoubleToken(buf.toString, 0.0))
            case c if c == 'F' || c == 'f' =>
                putc(c)
                done(FloatToken(buf.toString, 0.0f))
            case c =>
                ungetc(c)
                done(IntToken("0", BigInt(0)))
        }

        def decimalInt(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                acc = 10 * acc + (c - '0')
                tailcall(decimalInt())
            case '_' =>
                putc('_')
                tailcall(decimalSep())
            case '.' =>
                putc('.')
                tailcall(point())
            case c if c == 'e' || c == 'E' =>
                putc(c)
                tailcall(expMark())
            case c if c == 'L' || c == 'l' =>
                putc(c)
                done(LongToken(buf.toString, acc.toLong))
            case c if c == 'D' || c == 'd' =>
                putc(c)
                done(DoubleToken(buf.toString, acc.toDouble))
            case c if c == 'F' || c == 'f' =>
                putc(c)
                done(FloatToken(buf.toString, acc.toFloat))
            case c =>
                ungetc(c)
                done(IntToken(buf.toString, acc))
        }
        def decimalSep(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                acc = 10 * acc + (c - '0')
                tailcall(decimalInt())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }

        def octalInt(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '7' =>
                putc(c)
                acc = 8 * acc + (c - '0')
                tailcall(octalInt())
            case '_' =>
                putc('_')
                tailcall(octalSep())
            case c if c == 'L' || c == 'l' =>
                putc(c)
                done(LongToken(buf.toString, acc.toLong))
            case c =>
                ungetc(c)
                done(IntToken(buf.toString, acc))
        }
        def octalSep(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '7' =>
                putc(c)
                acc = 8 * acc + (c - '0')
                tailcall(octalInt())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }

        // state at "0b", "0B" or digit separator "_"
        def zeroB(): TailRec[Token] = getc() match {
            case c if c == '0' || c == '1' =>
                putc(c)
                acc = 2 * acc + (c - '0')
                tailcall(binaryInt())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }
        // reading binary integer 0[bB][01]+
        // if _ is used, 0[bB]([01]+_)+
        def binaryInt(): TailRec[Token] = getc() match {
            case c if c == '0' || c == '1' =>
                putc(c)
                acc = 2 * acc + (c - '0')
                tailcall(binaryInt())
            case '_' =>
                putc('_')
                tailcall(zeroB())
            case c =>
                ungetc(c)
                done(IntToken(buf.toString, acc))
        }

        // state at "0x", "0X" or digit separator "_"
        def zeroX(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                acc = 16 * acc + (c - '0')
                tailcall(hexInt())
            case c if 'A' <= c && c <= 'F' =>
                putc(c)
                acc = 16 * acc + (c - 'A' + 10)
                tailcall(hexInt())
            case c if 'a' <= c && c <= 'f' =>
                putc(c)
                acc = 16 * acc + (c - 'a' + 10)
                tailcall(hexInt())
            case '.' =>
                putc('.')
                tailcall(hexPoint())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }
        // reading hexadecimal integer 0[xX][0-9a-fA-F]+
        // if _ is used, 0[xX]([0-9a-fA-F]+_)+
        def hexInt(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                acc = 16 * acc + (c - '0')
                tailcall(hexInt())
            case c if 'A' <= c && c <= 'F' =>
                putc(c)
                acc = 16 * acc + (c - 'A' + 10)
                tailcall(hexInt())
            case c if 'a' <= c && c <= 'f' =>
                putc(c)
                acc = 16 * acc + (c - 'a' + 10)
                tailcall(hexInt())
            case '_' =>
                putc('_')
                tailcall(zeroX())
            case c if c == 'L' || c == 'l' =>
                putc(c)
                done(LongToken(buf.toString, acc.toLong))
            case '.' =>
                putc('.')
                tailcall(hexFloat())
            case c if c == 'p' || c == 'P' =>
                putc(c)
                tailcall(hexExpMark())
            case c =>
                ungetc(c)
                done(IntToken(buf.toString, acc))
        }

        // reading ".999" or ".hello()"
        def dot(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(frac())
            case c =>
                // Note: this parser does not recognize ".." operator
                ungetc(c)
                done(SymbolToken("."))
        }

        // Parsing floating point numbers
        //
        // Note1:
        // The variable acc is used only for integer literals.
        // This code delegates the floating point number parsing
        // to the String.{toFloat, toDouble, toBigDecimal} methods
        // because of the complexity.
        //
        // Note2:
        // BigDecimal(_:String) and String.{toFloat, toDouble} 
        // (and also BigInt(_:String), String.{toInt, toLong})
        // call java.lang.*.parse* methods;
        // but they do not accept underscores in the input strings.
        // So, we need to remove underscores
        // before calling these methods.

        // reading the point on "999."
        def point(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(frac())
            case c if c == 'e' || c == 'E' =>
                putc(c)
                tailcall(expMark())
            case c if c == 'd' || c == 'D' =>
                val dbl = buf.filter(_ != '_').toString.toDouble
                putc(c)
                val digits = buf.toString
                done(DoubleToken(digits, dbl))
            case c if c == 'f' || c == 'F' =>
                val flt = buf.filter(_ != '_').toString.toFloat
                putc(c)
                val digits = buf.toString
                done(FloatToken(digits, flt))
            case '_' =>
                putc('_')
                done(IllegalToken(buf.toString)) // malformed float
            case c =>
                ungetc(c)
                val digits = buf.toString
                val decimal = BigDecimal(digits.filter(_ != '_'))
                done(DecimalToken(digits, decimal))
        }

        // at the case of "999.9"
        def frac(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(frac())
            case '_' =>
                putc('_')
                tailcall(fracSep())
            case c if c == 'e' || c == 'E' =>
                putc(c)
                tailcall(expMark())
            case c if c == 'd' || c == 'D' =>
                putc(c)
                val digits = buf.toString
                val dbl = digits.filter(_ != '_').toDouble
                done(DoubleToken(digits, dbl))
            case c if c == 'f' || c == 'F' =>
                putc(c)
                val digits = buf.toString
                val flt = digits.filter(_ != '_').toFloat
                done(FloatToken(digits, flt))
            case c =>
                ungetc(c)
                val digits = buf.toString
                val decimal = BigDecimal(digits.filter(_ != '_'))
                done(DecimalToken(digits, decimal))
        }

        def fracSep(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(frac())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }

        // reading "999e" or "999E"
        def expMark(): TailRec[Token] = getc() match {
            case c if c == '+' || c == '-' =>
                putc(c)
                tailcall(expSign())
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(expPart())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }
        def expSign(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(expPart())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }
        def expPart(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(expPart())
            case '_' =>
                putc('_')
                tailcall(expSep())
            case c if c == 'd' || c == 'D' =>
                val dbl = buf.filter(_ != '_').toString.toDouble
                putc(c)
                val digits = buf.toString
                done(DoubleToken(digits, dbl))
            case c if c == 'f' || c == 'F' =>
                val flt = buf.filter(_ != '_').toString.toFloat
                putc(c)
                val digits = buf.toString
                done(FloatToken(digits, flt))
            case c =>
                ungetc(c)
                val digits = buf.toString
                val decimal = BigDecimal(digits.filter(_ != '_'))
                done(DecimalToken(digits, decimal))
        }
        def expSep(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(expPart())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }

        // reading fractional part of hexadecimal float
        // e.g. "0xff.", "0xff.f" and "0x.ff"
        def hexFloat(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(hexFloat())
            case c if 'A' <= c && c <= 'F' =>
                putc(c)
                tailcall(hexFloat())
            case c if 'a' <= c && c <= 'f' =>
                putc(c)
                tailcall(hexFloat())
            case '_' =>
                putc('_')
                tailcall(hexFloatSep())
            case c if c == 'p' || c == 'P' =>
                putc(c)
                tailcall(hexExpMark())
            // hexadecimal floats without 'P' are malformed
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }
        def hexFloatSep(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(hexFloat())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }

        // reading "0x."
        // following digit must be hexadecimal digit
        def hexPoint(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' ||
                      'A' <= c && c <= 'F' ||
                      'a' <= c && c <= 'f' =>
                putc(c)
                tailcall(hexFloat())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }

        // reading hexadecimal exponent mark 'P'
        // e.g. "0xf.ffp", "0xff.P", "0x.ff.p" and "0xFp"
        def hexExpMark(): TailRec[Token] = getc() match {
            case c if c == '+' || c == '-' =>
                putc(c)
                tailcall(hexExpSign())
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(hexExpPart())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }
        def hexExpSign(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(hexExpPart())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }
        def hexExpPart(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(hexExpPart())
            case '_' =>
                putc('_')
                tailcall(hexExpSep())
            case c if c == 'd' || c == 'D' =>
                val dbl = buf.filter(_ != '_').toString.toDouble
                putc(c)
                val digits = buf.toString
                done(DoubleToken(digits, dbl))
            case c if c == 'f' || c == 'F' =>
                val flt = buf.filter(_ != '_').toString.toFloat
                putc(c)
                val digits = buf.toString
                done(FloatToken(digits, flt))
            case c =>
                ungetc(c)
                val digits = buf.toString
                val dbl = digits.filter(_ != '_').toDouble
                done(DoubleToken(digits, dbl))
        }
        def hexExpSep(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' =>
                putc(c)
                tailcall(hexExpPart())
            case c =>
                ungetc(c)
                done(IllegalToken(buf.toString))
        }

        // parsing comments
        def slash(): TailRec[Token] = getc() match {
            // "//" line comment
            case '/' =>
                putc('/')
                tailcall(lineComment())
            // "/*" block comment
            case '*' =>
                putc('*')
                tailcall(blockComment())
            // "/=" operator
            case '=' =>
                putc('=')
                done(SymbolToken("/="))
            // "/" operator
            case c =>
                ungetc(c)
                done(SymbolToken("/"))
        }
        def lineComment(): TailRec[Token] = getc() match {
            case c if c == '\n' || c == '\r' || c == EOF =>
                done(CommentToken(buf.toString))
            case c =>
                putc(c)
                tailcall(lineComment())
        }
        def blockComment(): TailRec[Token] = getc() match {
            // detect "/* ... *"
            case '*' =>
                putc('*')
                tailcall(starInBlockComment())
            case EOF =>
                done(IllegalToken(buf.toString)) // unclosed block comment
            case c =>
                putc(c)
                tailcall(blockComment())
        }

        // "/* .... *"
        def starInBlockComment(): TailRec[Token] = getc() match {
            // "/* .... */"
            case '/' =>
                putc('/')
                done(CommentToken(buf.toString))
            // "/* ... **"
            case '*' =>
                putc('*')
                tailcall(starInBlockComment())
            case EOF =>
                done(IllegalToken(buf.toString)) // unclosed block comment
            case c =>
                putc(c)
                tailcall(blockComment())
        }

        // reading identifiers and keywords
        def alpha(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' ||
                      'A' <= c && c <= 'Z' ||
                      'a' <= c && c <= 'z' ||
                      c == '_' =>
                putc(c)
                tailcall(alnum())
            case c =>
                ungetc(c)
                done(IdToken(buf.toString))
        }
        def alnum(): TailRec[Token] = getc() match {
            case c if '0' <= c && c <= '9' ||
                      'A' <= c && c <= 'Z' ||
                      'a' <= c && c <= 'z' ||
                      c == '_' =>
                putc(c)
                tailcall(alnum())
            case c =>
                ungetc(c)
                done(IdToken(buf.toString))
        }

        start().result     // the return value of next()
    }
}
