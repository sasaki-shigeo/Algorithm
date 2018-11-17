public class DigitUtil {
    public static int atoi(CharSequence digits) {
        int n = 0;
        for (int i = 0; i < digits.length(); i++) {
            n = 10 * n + digits.charAt(i) - '0';
        }
        return n;
    }

    static int digitValue(int c) {
        if ('0' <= c && c <= '9')
            return c - '0';
        else if ('A' <= c && c <= 'Z')
            return c - 'A' + 10;
        else if ('a' <= c && c <= 'z')
            return c - 'a' + 10;
        else
            return -1;
    }

    public static int parseInt(CharSequence digits, int radix) {
        if (radix <= 1 || 36 < radix) {
            throw new IllegalArgumentException("radix: " + radix);
        }
    
        int n = 0;
        for (int i = 0; i < digits.length(); i++) {
            int d = digitValue(digits.charAt(i));
            if (0 <= d && d < radix) {
                n = radix * n + d;
            }
            else {
                throw new IllegalArgumentException("not in the range 0.." + radix + ": " + d);
            }
        }
        return n;
    }

    public static int parseInt(CharSequence digits) {
        return parseInt(digits, 10);
    }

    public static long praseLong(CharSequence digits, int radix) {
        if (radix <= 1 || 36 < radix) {
            throw new IllegalArgumentException("radix: " + radix);
        }
    
        long n = 0;
        for (int i = 0; i < digits.length(); i++) {
            int d = digitValue(digits.charAt(i));
            if (0 <= d && d < radix) {
                n = radix * n + d;
            }
            else {
                throw new IllegalArgumentException("not in the range 0.." + radix + ": " + d);
            }
        }
        return n;
    }

    public static long parseLong(CharSequence digits) {
        return parseLong(digits);
    }

    public static boolean isValidISBN10(String code) {
        int weight = 10;
        int sum = 0;
        int index = 0;
        if (code.startsWith("ISBN"))
            index = 4;

        while (index < code.length()) {
            int c = code.charAt(index++);
            if ('0' <= c && c <= '9') {
                sum += weight * (c - '0');
                --weight;
            }
            else if (weight == 1 && (c == 'x' || c == 'x')) {
                sum += 10;
                --weight;
            }
            else if (c == '-' || c == ' ')
                continue;
            else
                return false;
        }
        
        return weight == 0 && sum % 11 == 0;
    }

    public static boolean isValidISBN13(String code) {
        int sum = 0;
        int index = 0;
        int count = 0;
        if (code.startsWith("ISBN"))
            index = 4;

        while (index < code.length()) {
            int c = code.charAt(index++);
            if ('0' <= c && c <= '9') {
                sum += (2 * (count % 2) + 1) * (c - '0');
                // Note: 2 * (count % 2) + 1 == ((count & 1) << 1) + 1 is the weight.
                // 
                // count                | 0 1 2 3 4 5 6 7 8 9 10 11 12
                // ---------------------------------------------------
                // 2 * (count % 2) + 1  | 1 3 1 3 1 3 1 3 1 3  1  3  1
            }
            else if (c == '-' || c == ' ')
                continue;
            else
                return false;
        }
        
        return count == 12 && sum % 10 == 0;
    }

    public static char checkDigitISBN10(CharSequence code) {
        int weight = 10;
        int sum = 0;
        int index = 0;
        if (code.charAt(0) == 'I' &&
            code.charAt(1) == 'S' &&
            code.charAt(2) == 'B' &&
            code.charAt(3) == 'N') {
                index = 4;
        }

        while (index < code.length()) {
            int c = code.charAt(index++);
            System.out.printf("index = %d weight = %d c = %c\n", index, weight, c);
            if ('0' <= c && c <= '9') {
                sum += weight * (c - '0');
                --weight;
            }
            else if (c == '-' || c == ' ')
                continue;
            else {
                String message;
                if (0x21 <= c && c <= 0x7e)
                    message = String.format("Invalid character: %c", c);
                else
                    message = String.format("Invalid character: U+%4X", c);

                throw new IllegalArgumentException(message);
            }
        }
        
        if (weight == 1) {
            int mod = sum % 11;
            if (mod == 0)
                return '0';
            else if (mod == 1)
                return 'X';
            else
                return (char)('0' + 11 - mod);
        }
        else
            throw new IllegalArgumentException("Invalid size code: " + code);
    }

    public static char checkDigitISBN13(CharSequence code) {
        int sum = 0;
        int index = 0;
        int count = 0;
        if (code.charAt(0) == 'I' &&
            code.charAt(1) == 'S' &&
            code.charAt(2) == 'B' &&
            code.charAt(3) == 'N') {
                index = 4;
        }

        while (index < code.length()) {
            int c = code.charAt(index++);
            if ('0' <= c && c <= '9') {
                sum += (2 * (count % 2) + 1) * (c - '0');
                // Note: 2 * (count % 2) + 1 == ((count & 1) << 1) + 1 is the weight.
                // 
                // count                | 0 1 2 3 4 5 6 7 8 9 10 11 12
                // ---------------------------------------------------
                // 2 * (count % 2) + 1  | 1 3 1 3 1 3 1 3 1 3  1  3  1
            }
            else if (c == '-' || c == ' ')
                continue;
            else {
                String message;
                if (0x21 <= c && c <= 0x7e)
                    message = String.format("Invalid character: %c", c);
                else
                    message = String.format("Invalid character: U+%4X", c);

                throw new IllegalArgumentException(message);
            }
        }

        if (count == 12) {
            int mod = sum % 10;
            if (mod == 0)
                return '0';
            else
                return (char)('0' + 10 - mod);
        }
        else
            throw new IllegalArgumentException("Invalid size code: " + code);
    }

    public static void main(String[] args) {
        String code1 = "4-9876-5432-"; System.out.printf("Checkdigit of %s is %c\n", code1, checkDigitISBN10(code1));
        String code2 = "4-9876-5432";  System.out.printf("Checkdigit of %s is %c\n", code2, checkDigitISBN10(code2));
        String code3 = "4-9876-5432-6"; System.out.printf("%s is %s\n", code3, isValidISBN10(code3) ? "valid" : "invalid");
        String code4 = "4-00-010343"; System.out.printf("Checkdigit of %s is %c\n", code4, checkDigitISBN10(code4));
        String code5 = "4-00-010343-1"; System.out.printf("%s is %s\n", code5, isValidISBN10(code5) ? "valid" : "invalid");
        String code6 = "0-521-64176"; System.out.printf("Checkdigit of %s is %c\n", code6, checkDigitISBN10(code6));
        String code7 = "0-521-64176-4"; System.out.printf("%s is %s\n", code7, isValidISBN10(code7) ? "valid" : "invalid");
    }
}