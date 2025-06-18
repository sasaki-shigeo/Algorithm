// ParseInt test cases
//
//> using test.dep org.scalameta::munit::1.1.1
//> using file scanner.scala
//

package scanner

import munit.FunSuite
import java.io.{Reader, StringReader, PushbackReader}

def scanOnce(input: String): Token = {
  val automaton = new StateMachine(input)
  automaton.next()
}

class scannerTest extends FunSuite {
  test("scanOnce() in scanner.test.scala") {
    assertEquals(scanOnce(""), EOFToken)
    assertEquals(scanOnce("0"), IntToken("0", BigInt(0)))
  }

  test("decimal and octal integers") {
    assertEquals(scanOnce("1"), IntToken("1", BigInt(1)))
    assertEquals(scanOnce("123"), IntToken("123", BigInt(123)))
    assertEquals(scanOnce("077"), IntToken("077", BigInt(63)))
  }

  test("hexadecimal and binary integers") {
    assertEquals(scanOnce("0x0"), IntToken("0x0", BigInt(0)))
    assertEquals(scanOnce("0xFF"), IntToken("0xFF", BigInt(255)))
    assertEquals(scanOnce("0xff"), IntToken("0xff", BigInt(255)))
    assertEquals(scanOnce("0XA0"), IntToken("0XA0", BigInt(160)))
    assertEquals(scanOnce("0xFF_FF"), IntToken("0xFF_FF", BigInt(65535)))
    assertEquals(scanOnce("0b0"), IntToken("0b0", BigInt(0)))
    assertEquals(scanOnce("0b0000"), IntToken("0b0000", BigInt(0)))
    assertEquals(scanOnce("0B1111"), IntToken("0B1111", BigInt(15)))
    assertEquals(scanOnce("0b1111_0000"), IntToken("0b1111_0000", BigInt(240)))
  }

  test("'0x[EOF]', '0b[EOF]' and so on generate errors") {
    assertEquals(scanOnce("0x"), IllegalToken("0x"))
    assertEquals(scanOnce("0b"), IllegalToken("0b"))
    // assertEquals(scanOnce("0xG"), IllegalToken("0xG"))
    assertEquals(scanOnce("0x_"), IllegalToken("0x"))
  }

  test("integers with underscores") {
    assertEquals(scanOnce("1_000"), IntToken("1_000", BigInt(1000)))
    assertEquals(scanOnce("0_000"), IntToken("0_000", BigInt(0)))
    assertEquals(scanOnce("0xFF_FF"), IntToken("0xFF_FF", BigInt(65535)))
    assertEquals(scanOnce("0b1111_0000"), IntToken("0b1111_0000", BigInt(240)))

    assertEquals(scanOnce("1000_"), IllegalToken("1000_"))
  }

  test("floating point numbers") {
    // Note:
    // 123.456, 6.02E23, 6.626E-34, .999 and so on are inexact
    // that means the numbers possibly have rounding errors;
    // It is neccessary to give string literal to BigDecimal constructor.
    assertEquals(scanOnce("0.0"), DecimalToken("0.0", BigDecimal(0)))
    assertEquals(scanOnce("1.0"), DecimalToken("1.0", BigDecimal(1)))
    assertEquals(scanOnce("123.456"), DecimalToken("123.456", BigDecimal("123.456")))
    assertEquals(scanOnce("1.024e3"), DecimalToken("1.024e3", BigDecimal(1024)))
    assertEquals(scanOnce("6.02E23"), DecimalToken("6.02E23", BigDecimal("6.02e23")))
    assertEquals(scanOnce("6.626E-34"), DecimalToken("6.626E-34", BigDecimal("6.626e-34")))
    assertEquals(scanOnce("1e10"), DecimalToken("1e10", BigDecimal(1E10)))
    assertEquals(scanOnce("7E-9"), DecimalToken("7E-9", BigDecimal("7e-9")))
    assertEquals(scanOnce(".999"), DecimalToken(".999", BigDecimal(".999")))
    assertEquals(scanOnce(".0"), DecimalToken(".0", BigDecimal(0.0)))
    assertEquals(scanOnce("1."), DecimalToken("1.", BigDecimal(1.0)))
    assertEquals(scanOnce(".999E3"), DecimalToken(".999E3", BigDecimal(999)))
    assertEquals(scanOnce("1.E3"), DecimalToken("1.E3", BigDecimal(1000)))
    assertEquals(scanOnce("3.1415_92653_58979"),
                          DecimalToken("3.1415_92653_58979", BigDecimal("3.14159265358979")))

    assertEquals(scanOnce("1.0f"), FloatToken("1.0f", 1.0f))
    assertEquals(scanOnce("6.02e23f"), FloatToken("6.02e23f", 6.02e23f))
    assertEquals(scanOnce(".0f"), FloatToken(".0f", 0.0f))
    assertEquals(scanOnce("1f"), FloatToken("1f", 1.0f))

    assertEquals(scanOnce("1.0d"), DoubleToken("1.0d", 1.0))
    assertEquals(scanOnce("6.02e23d"), DoubleToken("6.02e23d", 6.02e23))
    assertEquals(scanOnce(".0d"), DoubleToken(".0d", 0.0))
    assertEquals(scanOnce("1d"), DoubleToken("1d", 1.0))
  }

  test("illegal floating point numbers") {
    assertEquals(scanOnce("1e"), IllegalToken("1e"))
    assertEquals(scanOnce("1E"), IllegalToken("1E"))
    assertEquals(scanOnce("1e-"), IllegalToken("1e-"))
    assertEquals(scanOnce("1E-"), IllegalToken("1E-"))
    assertEquals(scanOnce("1e+"), IllegalToken("1e+"))
    assertEquals(scanOnce("1E+"), IllegalToken("1E+"))
    assertEquals(scanOnce(".e10"), SymbolToken("."))
    assertEquals(scanOnce("3.14159_"), IllegalToken("3.14159_"))
    assertEquals(scanOnce("3._14159"), IllegalToken("3._"))
  }

  test("hexadecimal floating point numbers") {
    assertEquals(scanOnce("0x1.0p0"), DoubleToken("0x1.0p0", 1.0))
    assertEquals(scanOnce("0x1.8p2"), DoubleToken("0x1.8p2", 6.0))
    assertEquals(scanOnce("0xe.0P-3"), DoubleToken("0xe.0P-3", 1.75))
    assertEquals(scanOnce("0XffP-2"), DoubleToken("0XffP-2", 63.75))
    assertEquals(scanOnce("0x.fP3"), DoubleToken("0x.fP3", 7.5))

    assertEquals(scanOnce("0x1.0p0f"), FloatToken("0x1.0p0f", 1.0f))
    assertEquals(scanOnce("0x1.8p2f"), FloatToken("0x1.8p2f", 6.0f))
    assertEquals(scanOnce("0xe.0P-3f"), FloatToken("0xe.0P-3f", 1.75f))
    assertEquals(scanOnce("0XffP-2f"), FloatToken("0XffP-2f", 63.75f))
    assertEquals(scanOnce("0x.fP3f"), FloatToken("0x.fP3f", 7.5f))

    assertEquals(scanOnce("0x1.0p0d"), DoubleToken("0x1.0p0d", 1.0))
    assertEquals(scanOnce("0x1.8p2d"), DoubleToken("0x1.8p2d", 6.0))
    assertEquals(scanOnce("0xe.0P-3d"), DoubleToken("0xe.0P-3d", 1.75))
    assertEquals(scanOnce("0XffP-2d"), DoubleToken("0XffP-2d", 63.75))
    assertEquals(scanOnce("0x.fP3d"), DoubleToken("0x.fP3d", 7.5))

    assertEquals(scanOnce("0x1.2"), IllegalToken("0x1.2"))
    assertEquals(scanOnce("0x1.2pf"), IllegalToken("0x1.2p"))
    assertEquals(scanOnce("0x1.2P"), IllegalToken("0x1.2P"))
    assertEquals(scanOnce("0x1.2p+"), IllegalToken("0x1.2p+"))
    assertEquals(scanOnce("0x1.2P-"), IllegalToken("0x1.2P-"))
    assertEquals(scanOnce("0x1.2pff"), IllegalToken("0x1.2p"))
  }

  test("comments") {
    assertEquals(scanOnce("// comment\n"), CommentToken("// comment"))
    assertEquals(scanOnce("// comment"), CommentToken("// comment"))
    assertEquals(scanOnce("/* multi-line\ncomment */"), CommentToken("/* multi-line\ncomment */"))
    assertEquals(scanOnce("/***/"), CommentToken("/***/"))
    assertEquals(scanOnce("/* unclosed comment"), IllegalToken("/* unclosed comment"))
  }

  test("identifiers") {
    assertEquals(scanOnce("a"), IdToken("a"))
    assertEquals(scanOnce("abc123"), IdToken("abc123"))
    assertEquals(scanOnce("snake_case"), IdToken("snake_case"))
    assertEquals(scanOnce("_privateVar"), IdToken("_privateVar"))
    assertEquals(scanOnce("variableName"), IdToken("variableName"))
    assertEquals(scanOnce("ClassName"), IdToken("ClassName"))
  }

  test("punctuations") {
    assertEquals(scanOnce("!"), SymbolToken("!"))
    assertEquals(scanOnce("!="), SymbolToken("!="))
    assertEquals(scanOnce("!!"), SymbolToken("!"))
    assertEquals(scanOnce("%"), SymbolToken("%"))
    assertEquals(scanOnce("%="), SymbolToken("%="))
    assertEquals(scanOnce("&"), SymbolToken("&"))
    assertEquals(scanOnce("&="), SymbolToken("&="))
    assertEquals(scanOnce("&&"), SymbolToken("&&"))
    assertEquals(scanOnce("&&="), SymbolToken("&&="))
    assertEquals(scanOnce("&&!"), SymbolToken("&&"))
    assertEquals(scanOnce("&~"), SymbolToken("&"))   
    assertEquals(scanOnce("("), SymbolToken("("))
    assertEquals(scanOnce(")"), SymbolToken(")"))
    assertEquals(scanOnce("*"), SymbolToken("*"))
    assertEquals(scanOnce("*="), SymbolToken("*="))
    assertEquals(scanOnce(","), SymbolToken(","))
    assertEquals(scanOnce("+"), SymbolToken("+"))
    assertEquals(scanOnce("+="), SymbolToken("+="))
    assertEquals(scanOnce("++"), SymbolToken("++"))
    assertEquals(scanOnce("++="), SymbolToken("++="))
    assertEquals(scanOnce("-"), SymbolToken("-"))
    assertEquals(scanOnce("-="), SymbolToken("-="))
    assertEquals(scanOnce("--"), SymbolToken("--"))
    assertEquals(scanOnce("--="), SymbolToken("--="))
    // assertEquals(scanOnce("->"), SymbolToken("->"))
    assertEquals(scanOnce(":"), SymbolToken(":"))
    assertEquals(scanOnce(";"), SymbolToken(";"))
    assertEquals(scanOnce("<"), SymbolToken("<"))
    assertEquals(scanOnce("<<"), SymbolToken("<<"))
    assertEquals(scanOnce("<<="), SymbolToken("<<="))
    assertEquals(scanOnce("<="), SymbolToken("<="))
    assertEquals(scanOnce("="), SymbolToken("="))
    assertEquals(scanOnce("=="), SymbolToken("=="))
    // assertEquals(scanOnce("=>"), SymbolToken("=>"))
    assertEquals(scanOnce("=-"), SymbolToken("="))
    assertEquals(scanOnce(">"), SymbolToken(">"))
    assertEquals(scanOnce(">="), SymbolToken(">="))
    assertEquals(scanOnce(">>"), SymbolToken(">>"))
    assertEquals(scanOnce(">>="), SymbolToken(">>="))
    assertEquals(scanOnce(">>>"), SymbolToken(">>>"))
    assertEquals(scanOnce(">>>="), SymbolToken(">>>="))
    assertEquals(scanOnce("?"), SymbolToken("?"))
    assertEquals(scanOnce("["), SymbolToken("["))
    assertEquals(scanOnce("]"), SymbolToken("]"))
    assertEquals(scanOnce("{"), SymbolToken("{"))
    assertEquals(scanOnce("}"), SymbolToken("}"))
    assertEquals(scanOnce("|"), SymbolToken("|"))
    assertEquals(scanOnce("|="), SymbolToken("|="))
    assertEquals(scanOnce("|~"), SymbolToken("|"))
    assertEquals(scanOnce("||"), SymbolToken("||"))
    assertEquals(scanOnce("||="), SymbolToken("||="))
    assertEquals(scanOnce("||!"), SymbolToken("||"))
    assertEquals(scanOnce("~"), SymbolToken("~"))
    assertEquals(scanOnce("~="), SymbolToken("~="))
    assertEquals(scanOnce("^"), SymbolToken("^"))
    assertEquals(scanOnce("^="), SymbolToken("^="))

    assertEquals(scanOnce("."), SymbolToken("."))
    // assertEquals(scanOnce(".."), SymbolToken(".."))
    assertEquals(scanOnce("/"), SymbolToken("/"))
    assertEquals(scanOnce("/="), SymbolToken("/="))
    // assertEquals(scanOnce("::"), SymbolToken("::"))
    // assertEquals(scanOnce(":::"), SymbolToken(":::"))
    // assertEquals(scanOnce("<-"), SymbolToken("<-"))
  }
}