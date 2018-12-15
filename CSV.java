import java.io.*;
import java.util.*;

enum State {
    SEPERATOR, CELL, QUOTED_CELL, QUOTED_QUOTE
}

public class CSV {
    static final int EOF = -1;

    public static List<List<String>> read(Reader r) throws IOException {
        List<List<String>> table = new ArrayList<List<String>>();
        List<String> row = new ArrayList<String>();
        StringBuilder buf = new StringBuilder();
        int c;

        State st = State.SEPERATOR;
        for (;;) {
            switch (st) {
                case SEPERATOR:
                    switch (c = r.read()) {
                        case ',':
                            row.add("");
                            continue;
                        case '\"':
                            buf.setLength(0);
                            st = State.QUOTED_CELL;
                            continue;
                        case '\n':
                            row.add("");
                            table.add(row);
                            row = new ArrayList<String>();
                            continue;
                        case EOF:
                            row.add("");
                            table.add(row);
                            return table;
                        default:
                            buf.setLength(0);
                            buf.appendCodePoint(c);
                            st = State.CELL;
                            continue;
                    }
                case CELL:
                    switch (c = r.read()) {
                        case ',':
                            row.add(buf.toString());
                            st = State.SEPERATOR;
                            continue;
                        case '\n':
                            row.add(buf.toString());
                            table.add(row);
                            row = new ArrayList<String>();
                            st = State.SEPERATOR;
                            continue;
                        case EOF:
                            row.add(buf.toString());
                            table.add(row);
                            return table;
                        default:
                            buf.appendCodePoint(c);
                            continue;
                    }
                case QUOTED_CELL:
                    switch (c = r.read()) {
                        case '\"':
                            st = State.QUOTED_QUOTE;
                            continue;
                        case EOF:
                            row.add(buf.toString());
                            table.add(row);
                            return table;
                        default:
                            buf.appendCodePoint(c);
                            continue;
                    }
                case QUOTED_QUOTE:
                    switch (c = r.read()) {
                        case ',':
                            row.add(buf.toString());
                            st = State.SEPERATOR;
                            continue;
                        case '\n':
                            row.add(buf.toString());
                            table.add(row);
                            row = new ArrayList<String>();
                            st = State.SEPERATOR;
                            continue;
                        case '\"':
                            buf.append('\"');
                            st = State.QUOTED_CELL;
                            continue;
                        default:
                            throw new IllegalArgumentException("Cell not enclosed by quotation marks");
                    }
            }
        }
    }

    static final boolean hasQuote(String str) {
        return str.indexOf('\"') >= 0;
    }

    static final CharSequence quoteIfNecessary(Object x) {
        String str = x.toString();
        if (hasQuote(str)) {
            StringBuilder buf = new StringBuilder();
            buf.append('\"');
            for (int i = 0; i < str.length(); i++) {
                if (str.charAt(i) == '\"') {
                    buf.append("\"\"");
                }
                else {
                    buf.append(str.charAt(i));
                }
            }
            return buf;
        }
        else {
            return str;
        }
    }

    public static void write(Writer w, List<List<Object>> table) throws IOException {
        for (List<Object> row: table) {
            boolean firstColumn = true;
            for (Object cell: row) {
                if (! firstColumn) {
                    w.append(',');
                }
                w.append(quoteIfNecessary(cell));
            }
            w.append('\n');
        }
        w.close();
    }

    public static void main(String[] args) throws IOException {
        StringReader r = new StringReader("123, 456, 789");
        List<List<String>> table = read(r);
        System.out.println(table);
    }
}