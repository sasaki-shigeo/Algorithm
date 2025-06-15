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

  test("floating point numbers") {
    assertEquals(scanOnce("0.0"), DecimalToken("0.0", BigDecimal(0)))
    assertEquals(scanOnce("1.0"), DecimalToken("1.0", BigDecimal(1)))
    assertEquals(scanOnce("123.456"), DecimalToken("123.456", BigDecimal("123.456")))
    assertEquals(scanOnce("1.024e3"), DecimalToken("1.024e3", BigDecimal(1024)))
    assertEquals(scanOnce("6.02E23"), DecimalToken("6.02E23", BigDecimal("6.02e23")))
    assertEquals(scanOnce("6.626E-34"), DecimalToken("6.626E-34", BigDecimal("6.626e-34")))
    assertEquals(scanOnce("1e10"), DecimalToken("1e10", BigDecimal("1E10")))
    assertEquals(scanOnce("7E-9"), DecimalToken("7E-9", BigDecimal("7e-9")))
    assertEquals(scanOnce(".999"), DecimalToken(".999", BigDecimal(".999")))
    assertEquals(scanOnce("1."), DecimalToken("1.", BigDecimal("1.0")))
    assertEquals(scanOnce(".999E3"), DecimalToken(".999E3", BigDecimal(999)))
    assertEquals(scanOnce("1.E3"), DecimalToken("1.E3", BigDecimal(1000)))
  }

  test("illegal floating point numbers") {
    assertEquals(scanOnce("1e"), IllegalToken("1e"))
    assertEquals(scanOnce("1E"), IllegalToken("1E"))
    assertEquals(scanOnce("1e-"), IllegalToken("1e-"))
    assertEquals(scanOnce("1E-"), IllegalToken("1E-"))
    assertEquals(scanOnce("1e+"), IllegalToken("1e+"))
    assertEquals(scanOnce("1E+"), IllegalToken("1E+"))
  }

  test("comments") {
    assertEquals(scanOnce("// comment\n"), CommentToken("// comment"))
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
}