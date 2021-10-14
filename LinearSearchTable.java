import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.Iterator;
import java.util.AbstractMap;
import java.util.AbstractSet;

public class LinearSearchTable<K, V> extends AbstractMap<K, V> {

    class Entry extends SimpleEntry<K, V> {
        K key;
        V value;

        public Entry(K key, V value) {
            super(key, value);
            this.key = key;
            this.value = value;
        }
    }

    protected ArrayList<Map.Entry<K,V>> table_;

    public LinearSearchTable() {
        table_ = new ArrayList<>();
    }

    public LinearSearchTable(int n) {
        table_ = new ArrayList<>(n);
    }

    public Set<Map.Entry<K, V>> entrySet() {
	return new AbstractSet<Map.Entry<K, V>>() {
	    public int size() {
		return table_.size();
	    }

	    public Iterator<Map.Entry<K, V>> iterator() {
		return table_.iterator();
	    }
	};
    }    

    @Override
    public void clear() {
        table_.clear();
    }

    protected int lin_search(Object key) {
        if (key == null)
            return -1;

        for (int ix = 0; ix < table_.size(); ix++) {
            if (key.equals(table_.get(ix).getKey()))
                return ix;
        }
        return -1;
    }

    protected int index(Object key) {
        return lin_search(key);
    }

    @Override
    public V get(Object key) {
        int ix = index(key);
        if (ix >= 0)
            return table_.get(ix).getValue();
        else
            return null;
    }

    @Override
    public V put(K key, V value) {
        int ix = index(key);
        if (ix >= 0) {
            Map.Entry<K, V> ent = table_.get(ix);
            V before = ent.getValue();
            ent.setValue(value);
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
