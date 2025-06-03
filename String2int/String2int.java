import java.io.Reader;
import java.io.StringReader;
import java.io.IOException;

enum State { START, ZERO, DEC, OCT, X, HEX } 

/** 
 * String2int is a utility class that provides methods to convert
 * strings representing numbers in various bases (decimal, octal, hexadecimal)
 * to their integer and long equivalents.
 */
public class String2int {

    /**
     * Converts a string representing a number in decimal, octal, or hexadecimal
     * format to an integer.
     *
     * @param digits the string representation of the number
     * @return the integer value of the number
     */
    public static int string2int(String digits) {
        return (int)string2long(digits);
    }

    /** 
     * Converts a string representing a number in decimal, octal, or hexadecimal
     * format to a long.
     * 
     * @param digits the string representation of the number
     * @return the long value of the number
     */
    public static long string2long(String digits) {
        return parseLong(new StringReader(digits));
    }

    /**
     * Wrapper function for parseLong
     * 
     * This function reads a numeric literal from a Reader and returns its integer value
     * instead of long.
     * 
     * @param r the Reader containing the numeric literal
     * @return the integer value of the numeric literal
     */
    public static int parseInt(Reader r) {
        return (int)parseLong(r);
    }

    /**
     * Parses a numeric literal from a Reader and returns its long value.
     * The numeric literal can be in decimal, octal, or hexadecimal format.
     *
     * @param r the Reader containing the numeric literal
     * @return the long value of the numeric literal
     */    
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
                        else if ('0' <= c && c <= '9') {
                            st = State.DEC;
                            result = c - '0';
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
                        if ('0' <= c && c <= '9') {
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
                        if ('0' <= c && c <= '9') {
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
                        if ('0' <= c && c <= '9') {
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
        String[] testData = { "0", "123", "077", "0xff", "0XFF" };
        for (String digit: testData) {
            System.out.printf("%s = %d%n", digit, string2int(digit));
        }
    }
}
