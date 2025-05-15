import java.io.Reader
import java.io.StringReader

enum State { case START, ZERO, DEC, OCT, BIN, X, HEX }


class StringIterator(cs: CharSequence) extends Iterator[Char] {
    var ix = 0
    def hasNext: Boolean = ix < cs.length
    def next(): Char = {
        val c = cs.charAt(ix)
        ix += 1
        c
    }
}

class ReaderIterator(reader: Reader) extends Iterator[Char] {
    val EOF = -1               // the mark of End of File   
    var c = reader.read()
    def hasNext: Boolean = (c != EOF)
    def next(): Char = {
        val result = c.toChar
        c = reader.read()
        result
    }
}

class ParseInt(it: Iterator[Char]) {
    var st  = State.START
    var result = 0L

    def this(cs: CharSequence) = {
        this(new StringIterator(cs))
    }

    def this(reader: Reader) = {
        this(new ReaderIterator(reader))
    }

    // stub is this now
    def parseLong(): Long = {
        for (c <- it) {
            val action: Function0[Unit] = transitionTable(st, c)
            action()
        }
        result
    }
    
    def toZERO(): Unit = {
        st = State.ZERO
    }

    def toDEC(): Unit = {
        result = 10 * result + (c - '0')
        st = State.DEC
    }

    def toBIN(): Unit = {
        result = 2 * result + (c - '0')
        st = State.BIN
    }

    def toX(): Unit = {
        st = State.X
    }

    def toHEX(): Unit = {
        val d = if 'A' <= c && c <= 'F' then c - 'A' + 10
                else if 'a' <= c && c <= 'f' then c - 'a' + 10
                else c - '0'
        
        result = 16 * result + d
        st = State.HEX
    }
}