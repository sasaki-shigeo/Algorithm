import java.util.AbstractList;

public class VArray<T> extends AbstractList<T> {
    protected T[] array_;
    protected int limit = 0;

    public VArray(int n) {
        array_ = (T [])new Object[n];
    }

    public VArray() {
        this(10);
    }

    public int capacity() {
        return array_.length;
    }

    public int size() {
        return limit;
    }

    public T get(int n) {
        return array_[n];
    }

    public T set(int n, T x) {
        T before = array_[n];
        array_[n] = x;
        return before;
    }

    public boolean add(T x) {
        if (limit >= capacity()) {
            T [] newArray = (T [])new Object[2 * limit];
            for (int i = 0; i < limit; i++) {
                newArray[i] = array_[i];
            }
            array_ = newArray;
        }

        array_[limit++] = x;
        return true;
    }

    public static void main(String[] args) {
        VArray<Integer> xs = new VArray<>();
        xs.add(1);
        xs.add(2);
        xs.add(4);
        xs.add(8);
        System.out.println(xs);
    }
}
