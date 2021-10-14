import java.util.ArrayList;
import java.util.Map;
import java.util.AbstractMap.SimpleEntry;

public class LinearSearchTable<K, V> {

    class Entry extends SimpleEntry<K, V> {
        K key;
        V value;

        public Entry(K key, V value) {
            super(key, value);
            this.key = key;
            this.value = value;
        }
    }

    protected ArrayList<Entry> table_;

    public LinearSearchTable() {
        table_ = new ArrayList<>();
    }

    public LinearSearchTable(int n) {
        table_ = new ArrayList<>(n);
    }

    public int size() {
        return table_.size();
    }

    public void clear() {
        table_.clear();
    }

    protected int lin_search(K key) {
        if (key == null)
            return -1;

        for (int ix = 0; ix < table_.size(); ix++) {
            if (key.equals(table_.get(ix).key))
                return ix;
        }
        return -1;
    }

    protected int index(K key) {
        return lin_search(key);
    }

    public V get(K key) {
        int ix = index(key);
        if (ix >= 0)
            return table_.get(ix).value;
        else
            return null;
    }

    public V put(K key, V value) {
        int ix = index(key);
        if (ix >= 0) {
            Entry ent = table_.get(ix);
            V before = ent.value;
            ent.value = value;
            return before;
        }
        else {
            table_.add(new Entry(key, value));
            return null;
        }
    }

    // Test code will be here
    public static void main(String[] args) {

    }
}
