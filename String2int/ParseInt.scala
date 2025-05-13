import java.io.Reader
import java.io.StringReader

sealed class State {
    val EOF = -1
    def transit(c: Char): State
}

case object START extends State {
    def transit(c: Char): State = {
        if (c == '0') {
            ZERO
        }
        else if ('1' <= c && c <= '9') {
            DEC
        }
    }
}
case class ZERO  extends State
case class DEC   extends State
case class OCT   extends State
case class X     extends State
case class HEX   extends State