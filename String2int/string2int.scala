//
// Description: A Scala implementation of a parser for integer literals
//

package string2int

// The states of the parser
enum State { case START, ZERO, DEC, OCT, B, BIN, X, HEX, ERROR }

import State._

// The class of the transition table entries.
case class TransitionEntry(action: Function1[Char, Unit], nextState: State)

def parseInt(str: String): Int = {
    val parser = new StateMachine(str)
    parser.parseInt()
}

def parseLong(str: String): Long = {
    val parser = new StateMachine(str)
    parser.parseLong()
}

class StateMachine(input: String) {
    var st  = State.START
    var acc = 0L

    def parseLong(): Long = {
        st = START
        acc = 0L
        for (ch <- input) {
            val TransitionEntry(action, nextState) = transitionTable(st, ch)
            action(ch)
            st = nextState
        }
        if (st == ERROR || st == X || st == B) {
            throw new NumberFormatException(s"Invalid input: $input")
        }

        acc   // result value
    }

    def parseInt(): Int = {
        val result = parseLong()
        if (result < Int.MinValue || result > Int.MaxValue) {
            throw new NumberFormatException(s"Value out of range for int: $result")
        }
        result.toInt
    }

    def doNothing(ch: Char): Unit = { /* do nothing */ }
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

    // This function decides the action and the next state at runtime.
    // while the transition table is built at compile time.
    def transitionFunction(st: State, ch: Char): TransitionEntry = st match {
        case START => ch match {
            case '0' => TransitionEntry(doNothing(_), ZERO)
            case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateDecimal(_), DEC)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case ZERO => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => 
                TransitionEntry(accumulateOctal(_), OCT)
            case 'b' | 'B' => TransitionEntry(doNothing(_), B)
            case 'x' | 'X' => TransitionEntry(doNothing(_), X)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case DEC => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateDecimal(_), DEC)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case OCT => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' =>
                TransitionEntry(accumulateOctal(_), OCT)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case B => ch match {
            case '0' | '1' => TransitionEntry(accumulateBinary(_), BIN)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case BIN => ch match {
            case '0' | '1' => TransitionEntry(accumulateBinary(_), BIN)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case X => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateHexByDecimal(_), HEX)
            case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
                TransitionEntry(accumulateHexByUpper(_), HEX)
            case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' =>
                TransitionEntry(accumulateHexByLower(_), HEX)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case HEX => ch match {
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                TransitionEntry(accumulateHexByDecimal(_), HEX)
            case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
                TransitionEntry(accumulateHexByUpper(_), HEX)
            case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' =>
                TransitionEntry(accumulateHexByLower(_), HEX)
            case _ => TransitionEntry(doNothing(_), ERROR)
        }
        case ERROR => TransitionEntry(doNothing(_), ERROR) 
    }

    private val table = Vector.tabulate(State.values.size, 128) {
        (state_ix, char_ix) =>
        val st = State.values(state_ix)
        val ch = char_ix.toChar
        transitionFunction(st, ch)
    }

    // transition table build when compile time
    def transitionTable(st: State, ch: Char): TransitionEntry = {
        table(st.ordinal)(ch.toInt)
    }
}
