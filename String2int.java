import java.io.Reader;
import java.io.StringReader;
import java.io.IOException;
import static java.lang.Character.isDigit;
import static java.lang.Character.isDigit;

enum State { START, ZERO, DEC, OCT, X, HEX } 

public class String2int {
    public static int string2int(String digits) {
	return (int)string2long(digits);
    }

    public static long string2long(String digits) {
	return parseLong(new StringReader(digits));
    }

    public static int parseInt(Reader r) {
	return (int)parseLong(r);
    }

    public static long parseLong(Reader r) {
	State st = State.START; 
	long result = 0;
	int c = 0;
	final int EOF = -1;
	
	try {
	    while ((c = r.read()) != EOF) {
		switch (st) {
		    case START:
			if (c == '0') {
			    st = State.ZERO;
			    continue;
			}
			else if (isDigit(c)) {
			    st = State.DEC;
			    result = 'c' - '0';
			    continue;
			}
			else {
			    return result;
			}
		    case ZERO:
			if (c == 'x' || c == 'X') {
			    st = State.X;
			    continue;
			}
			else if ('0' <= c && c <= '7') {
			    st = State.OCT;
			    result = c - '0';
			    continue;
			}
			else {
			    return result;
			}
		    case DEC:
			if (isDigit(c)) {
			    result *= 10;
			    result += c - '0';
			    continue;
			}
			else {
			    return result;
			}
		    case OCT:
			if ('0' <= c && c <= '7') {
			    result *= 8;
			    result += c - '0';
			    continue;
			}
			else {
			    return result;
			}
		    case X:
			if (isDigit(c)) {
			    result = c - '0';
			    continue;
			}
			else if ('A' <= c && c <= 'F') {
			    result = c - 'A' + 10;
			    continue;
			}
			else if ('a' <= c && c <= 'f') {
			    result = c - 'a' + 10;
			    continue;
			}
			else {
			    throw new NumberFormatException("0x"+c);
			}
		    case HEX:
			if (isDigit(c)) {
			    result *= 16;
			    result += c - '0';
			    continue;
			}
			else if ('A' <= c && c <= 'F') {
			    result *= 16;
			    result += c - 'A' + 10;
			    continue;
			}
			else if ('a' <= c && c <= 'f') {
			    result *= 16;
			    result += c - 'a' + 10;
			    continue;
			}
			else {
			    return result;
			}
		    default:
			throw new RuntimeException("parseLong internal error");
		}
	    }
	} catch (IOException e) {
	    throw new NumberFormatException();
	}

	return result;
    }

    public static void main(String[] args) {
	System.out.printf("0xFF = %d%n", string2int("0xFF"));
    }
}
