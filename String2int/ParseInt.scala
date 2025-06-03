// 
// Description: A Scala implementation of a parser for integer literals
//

import java.io.Reader
import java.io.PushbackReader
import java.io.StringReader

trait PushbackIterator[T] extends Iterator[T] {
    /** Push back a character to the iterator.
     * 
     * This method allows the iterator to backtrack by one character.
     * It should throw an RuntimeException if the iterator cannot push back.
     */
    def pushback(x: T): Unit
}

/** An iterator that can push back a character.
 * 
 * This is used in the ParseInt class to allow the parser to backtrack
 * when it encounters an unexpected character.
 * 
 * @param cs the character sequence to iterate over
 * @throws java.io.IOException if the iterator is exhausted or unavailable to pushback
 */
class StringIterator(cs: CharSequence) extends PushbackIterator[Char] {
    var ix = 0

    def hasNext: Boolean = ix < cs.length
    def next(): Char = {
        // return cs[ix++]; in C language style
        val ch = cs.charAt(ix)
        ix += 1
        ch
    }
    def pushback(ch: Char): Unit = {
        if (ix > 0) {
            ix -= 1
            if (cs.charAt(ix) != ch) {
                throw new java.io.IOException("inconsistent pushback")
            }
        }
        else {
            throw new java.io.IOException("cannot back")
        }
    }
}

class ReaderIterator(reader: Reader) extends PushbackIterator[Char] {
    val EOF = -1               // the mark of End of File
    var buf: Option[Char] = None

    def hasNext: Boolean = buf match {
        case Some(_) => true
        case None => {
            val c = reader.read()
            c != EOF && { buf = Some(c.toChar); true }
        }
    }
    def next(): Char = buf match {
        case Some(ch) => {
            buf = None
            ch
        }
        case None => {
            val c = reader.read()
            if (c != EOF) {
                c.toChar
            }
            else {
                throw new java.io.IOException("Reader exhausted")
            }
        }
    }
    def pushback(ch: Char): Unit = buf match {
        case None => buf = Some(ch)
        case Some(_) => throw new java.io.IOException("cannot pushback")
    }
}

class PushbackReaderIterator(reader: PushbackReader) extends PushbackIterator[Char] {
    val EOF = -1               // the mark of End of File   
    var c = reader.read()
    def hasNext: Boolean = (c != EOF)
    def next(): Char = {
        val result = c.toChar
        c = reader.read()
        result
    }
    def pushback(ch: Char): Unit = {
        reader.unread(ch)
    }
}

// The states of the parser
enum State { case START, ZERO, DEC, OCT, B, BIN, X, HEX, FINISH, ERROR }

// The class of the transition table entries.
case class TransitionEntry(action: Function1[Char, Unit], nextState: State)

class ParseInt(it: PushbackIterator[Char]) {
    var st  = State.START
    var result = 0L     // mock; be removed later
    var acc = 0L

    def this(cs: CharSequence) = {
        this(new StringIterator(cs))
    }

    def this(reader: Reader) = {
        this(new ReaderIterator(reader))
    }

    // stub is this now
    def parseLong(): Long = {
        for (ch <- it) {
            val elem = transitionFunction(st, ch)
            elem.action(ch)
            st = elem.nextState
            if (st == State.FINISH) {
                return acc
            }
            else if (st == State.ERROR) {
                throw new NumberFormatException(s"Invalid character '$ch' in input")
            }
        }
        acc
    }

    /*
     * nextState(ST) is a syntactic gimmic.
     * 
     * This function must stay at the last line of the action functions;
     * and the return values of the action functions are the next states.
     * So it seams that nextState(ST) indicates the next state is ST.
     * 
     */
    inline def nextState(st: State): State = st

    def toZERO(ch: Char): State = {
        nextState(State.ZERO)
    }

    def toDEC(ch: Char): State = {
        result = 10 * result + (ch - '0')
        nextState(State.DEC)
    }

    def toBIN(ch: Char): State = {
        result = 2 * result + (ch - '0')
        nextState(State.BIN)
    }

    def toX(ch: Char): State = {
        nextState(State.X)
    }

    def toHEX(ch: Char): State = {
        val d = if 'A' <= ch && ch <= 'F' then ch - 'A' + 10
                else if 'a' <= ch && ch <= 'f' then ch - 'a' + 10
                else ch - '0'
        
        result = 16 * result + d
        nextState(State.HEX)
    }

    def doNothing(ch: Char): Unit = { /* do nothing */ }
    def pushback(ch: Char): Unit = {
        it.pushback(ch)
    }
    def accumulateDecimal(ch: Char): Unit = {
        acc = 10 * acc + (ch - '0')
    }
    def accumulateOctal(ch: Char): Unit = {
        acc = 8 * acc + (ch - '0')
    }
    def accumulateBinary(ch: Char): Unit = {
        acc = 2 * acc + (ch - '0')
    }
    def accumulateHexByDecimal(ch: Char): Unit = {
        acc = 16 * acc + (ch - '0')
    }
    def accumulateHexByUpper(ch: Char): Unit = {
        acc = 16 * acc + (ch - 'A' + 10)
    }
    def accumulateHexByLower(ch: Char): Unit = {
        acc = 16 * acc + (ch - 'a' + 10)
    }

    // This is a mock of the action table.
    // This function is executed at runtime
    // while an action table should  computed at compile time.
    def transitionFunction(st: State, ch: Char): TransitionEntry = st match {
        case State.START => ch match {
            case '0' => TransitionEntry(doNothing(_), State.ZERO)
            case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateDecimal(_), State.DEC)
            case _ => TransitionEntry(pushback(_), State.FINISH)
        }
        case State.ZERO => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => 
                TransitionEntry(accumulateOctal(_), State.OCT)
            case 'b' | 'B' => TransitionEntry(doNothing(_), State.B)
            case 'x' | 'X' => TransitionEntry(doNothing(_), State.X)
            case _ => TransitionEntry(pushback(_), State.FINISH)
        }
        case State.DEC => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateDecimal(_), State.DEC)
            case _ => TransitionEntry(pushback(_), State.FINISH)
        }
        case State.OCT => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' =>
                TransitionEntry(accumulateOctal(_), State.OCT)
            case _ => TransitionEntry(pushback(_), State.FINISH)
        }
        case State.B => ch match {
            case '0' | '1' => TransitionEntry(accumulateBinary(_), State.BIN)
            case _ => TransitionEntry(pushback(_), State.ERROR)
        }
        case State.BIN => ch match {
            case '0' | '1' => TransitionEntry(accumulateBinary(_), State.BIN)
            case _ => TransitionEntry(pushback(_), State.FINISH)
        }
        case State.X => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateHexByDecimal(_), State.HEX)
            case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
                TransitionEntry(accumulateHexByUpper(_), State.HEX)
            case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' =>
                TransitionEntry(accumulateHexByLower(_), State.HEX)
            case _ => TransitionEntry(pushback(_), State.ERROR)
        }
        case State.HEX => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateHexByDecimal(_), State.HEX)
            case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
                TransitionEntry(accumulateHexByUpper(_), State.HEX)
            case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' =>
                TransitionEntry(accumulateHexByLower(_), State.HEX)
            case _ => TransitionEntry(pushback(_), State.FINISH)
        }
        case State.FINISH => TransitionEntry(doNothing(_), State.FINISH)
        case State.ERROR => TransitionEntry(doNothing(_), State.ERROR) 
    }
}
