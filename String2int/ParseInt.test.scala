// ParseInt test cases
//
//> using test.dep org.scalameta::munit::1.1.1
//> using file ParseInt.scala
//

import munit.FunSuite
import java.io.{Reader, StringReader, PushbackReader}

class ParseIntTest extends FunSuite {

  test("StringIterator can pushback") {
    val it = new StringIterator("foo bar")
    val List(a, b, c, d, e, f, g) = it.toList
    assert(a == 'f' && b == 'o' && c == 'o' && d == ' ' && e == 'b' && f == 'a' && g == 'r')
    it.pushback('r')
    it.pushback('a')
    it.pushback('b')
    val List(h, i, j) = it.toList
    assert(h == 'b' && i == 'a' && j == 'r')

    intercept[java.io.IOException] {
      it.pushback('x') // pushing back a different character should throw an exception
    }
  }

  test("parseInt should correctly parse decimal numbers") {
    val input = new StringReader("123")
    val result = ParseInt(input).parseInt()
    assertEquals(result, 123)
  }

  test("parseInt should correctly parse octal numbers") {
    val input = new StringReader("077")
    val result = ParseInt(input).parseInt()
    assertEquals(result, 63)
  }

  test("parseInt should correctly parse hexadecimal numbers") {
    val input = new StringReader("0xFF")
    val result = ParseInt(input).parseInt()
    assertEquals(result, 255)
  }

  test("parseInt should throw NumberFormatException for invalid input".ignore) {
    val input = new StringReader("0x")
    intercept[NumberFormatException] {
      ParseInt(input).parseInt()
    }
  }
}
