import java.util.*;

public class HashTable<K, V> extends AbstractMap<K, V> {
    protected SimpleEntry<K, V>[] table_;
    protected int count_ = 0;
    private HashTable<K, V> self = this;

    public HashTable(int n) {
        table_ = (SimpleEntry<K, V>[])new SimpleEntry[n];
    }

    public HashTable() {
        this(10);
    }

    protected int hash(Object key) {
        return key.hashCode() & 0x3FFFFFFF;
    }
    protected int hash2(Object key) {
        return (key.hashCode() >>> 16) + 7;
    }

    @Override
    public int size() {
        return count_;
    }

    public int capacity() {
        return table_.length;
    }

    @Override
    public void clear() {
        for (int i = 0; i < capacity(); i++) {
            table_[i] = null;
        }
        count_ = 0;
    }

    final int MAX_RETRY = 10;

    int searchGET(Object key) {
        return searchGET(key, table_);
    }

    int searchGET(Object key, Map.Entry<K, V>[] table) {
        int ix = hash(key) % capacity();
        for (int i = 0; i < MAX_RETRY; i++) {
            if (table[ix] == null)
                return -1;
            else {
                K k = table[ix].getKey();
                if (k != null && k.equals(key)) 
                    return ix;
               
            }
            ix = (ix + hash2(key)) % capacity();
        }
        throw new RuntimeException("Hashkey loop");
    }

    int searchPUT(K key) {
        return searchPUT(key, table_);
    }

    int searchPUT(K key, Map.Entry<K, V>[] table) {
        int ix = hash(key) % capacity();

        for (int i = 0; i < MAX_RETRY; i++) {
            if (table[ix] == null)
                return ix;
            else {
                K k = table[ix].getKey();
                if (k == null || k.equals(key))
                    return ix;
            }
            ix = (ix + hash2(key)) % capacity();
        }
        return -1;
    }

    @Override
    public boolean containsKey(Object key) {
        return searchGET(key) >= 0;
    }

    @Override
    public V get(Object key) {
        int ix = searchGET(key);
        if (ix >= 0)
            return table_[ix].getValue();
        else
            return null;
    }

    @Override
    public V getOrDefault(Object key, V defaultValue) {
        int ix = searchGET(key);
        if (ix >= 0)
            return table_[ix].getValue();
        else
            return defaultValue;
    }

    @Override
    public V put(K key, V value) {
        if (size() > capacity() / 2)
            rehash();

        int ix = searchPUT(key);
        if (ix >= 0) {
            V result = get(key);

            if (table_[ix] == null)
                table_[ix] = new SimpleEntry<>(key, value);
            else
                table_[ix].setValue(value);

            count_++;
            return result;
        }
        else {
            System.err.println("Emergency rehash:" + key + ", " + value);
            rehash();
            put(key, value);
            return null;
        }
    }

    @Override
    public V remove(Object key) {
        int ix = searchGET(key);
        if (ix >= 0) {
            V result = table_[ix].getValue();
            table_[ix] = new SimpleEntry<>(null, null);
            // SimpleEnty(null, null), exactly SimpleEnty of which the key is null,
            // is the dummy entry that had been removed.
            // The method searchGET must search following entries
            // and searchPUT returns the index to the entry to register.
            return result;
        }
        else {
            return null;
        }

    }

    class EntryIterator implements Iterator<Map.Entry<K, V>> {
        private int ix = 0;
        
        EntryIterator() {
            findNext();
        }

        private void findNext() {
            while (ix < table_.length &&
                   (table_[ix] == null ||
                    table_[ix].getKey() == null)) {
                ix++;
            }
        }

        public boolean hasNext() {
            return ix < table_.length;
        }

        public Map.Entry<K, V> next() {
            if (! hasNext()) {
                throw new NoSuchElementException();
            }

            Map.Entry<K, V> result = table_[ix++];
            findNext();
            return result;
        }
    }

    public Set<Map.Entry<K, V>> entrySet() {
        return new AbstractSet<Map.Entry<K, V>>() {
            public int size() {
                return count_;
            }
            @Override
            public Iterator<Map.Entry<K, V>> iterator() {
                return new EntryIterator();
            }
        };
    }

    class KeyIterator implements Iterator<K> {
        EntryIterator it;

        KeyIterator() {
            it = new EntryIterator();
        }

        public boolean hasNext() {
            return it.hasNext();
        }

        public K next() {
            return it.next().getKey();
        }
    }

    public Set<K> keySet() {
        return new AbstractSet<K>() {
            public int size() {
                return count_;
            }
            @Override
            public Iterator<K> iterator() {
                return new KeyIterator();
            }

            public boolean contains(Object key) {
                return self.containsKey(key);
            }
        };
    }

    void rehash() {
        // System.err.println("rehash is called.");
        SimpleEntry<K, V>[] newTable;

        retry: do {
            newTable = (SimpleEntry<K, V>[])new SimpleEntry[2 * capacity() + 1];
            for (Map.Entry<K, V> entry :entrySet()) {
                int ix = searchPUT(entry.getKey(), newTable);
                if (ix < 0)
                    continue retry;

                newTable[ix] = (SimpleEntry<K, V>)entry;
            }
        } while (false);
        table_ = newTable;
    }

    public static void main(String[] args) {
        HashTable<String, String> table = new HashTable<>();
        table.put("Japan", "Tokyo");
        table.put("US", "Washington");
        table.put("UK", "London");
        table.put("France", "Paris");
        table.put("Italy", "Rome");
        table.put("Germany", "Berlin");
        table.put("Russia", "Moscow");
        table.put("Holand", "Amsterdom");
        System.out.println(table);
        table.put("Spain", "Madrid");
        table.put("Denmark", "Copenhagen");
        table.put("Greece", "Athen");
        System.out.println(table);
        table.put("Soviet", "Moscow");
        table.remove("Soviet");

        for (String country: table.keySet()) {
            System.out.printf("%s: %8x %d %d\n", country, country.hashCode(), table.hash(country) % 10, table.hash2(country) % 10);
            System.out.printf("%s: %8x %d %d\n", country, country.hashCode(), table.hash(country) % 21, table.hash2(country) % 21);
        }
    }
}
