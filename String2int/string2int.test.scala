// ParseInt test cases
//
//> using test.dep org.scalameta::munit::1.1.1
//> using file string2int.scala
//

import munit.FunSuite
import string2int._

class ParseIntTest extends FunSuite {

  test("0") {
    assertEquals(parseInt("0"), 0)
  }

  test("00") {
    assertEquals(parseInt("00"), 0  )
  }

  test("99") {
    assertEquals(parseInt("99"), 99)
  }

  test("077 (octal)") {
    assertEquals(parseInt("077"), 63)
  }

  test("0b101010 (binary)") {
    assertEquals(parseInt("0b101010"), 42)
  }

  test("0x2A (hexadecimal)") {
    assertEquals(parseInt("0x2A"), 42)
  }

  test("'0x' causes an error") {
    intercept[NumberFormatException] {
      parseInt("0x")
    }
  }
}
