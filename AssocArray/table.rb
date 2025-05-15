class Table
    class KeyValue
        # creates instance variables @key and @value;
        # defines getter / setter as name as obj.key and obj.value (note: no '@')
        attr_accessor :key, :value

        # KeyValue.new(key, value) creates a new instance
        def initialize(key, value)
            @key, @value = key, value
        end

        def to_a
            [@key, @value]
        end

        def to_s
            '(' + @key + ', ' + @value + ')'
        end

        def to_str
            @key.to_s + ': ' + @value.to_s
        end
    end

    def initialize(n = 0)
        @table = Array.new(n)
    end

    @table = []              # the list of key-value pairs

    def to_a
        @table
    end

    def to_s
        '{' + @table.join(', ') + '}'
    end

    #
    # utility funtion of the linear search 
    #
    # Note: private functions in Ruby are visible to subclasses
    #
    private def _find(key)
        @table.each_index{ |ix|
            if @table[ix].key == key
                return ix
            end
        }
        return -1
    end

    #
    # Methods for Associative Arrays (as known as Dictionaries or Map)
    #

    def clear
        @table = []
    end

    def length
        return @table.length
    end

    def [](key)
        ix = _find(key)
        if ix >= 0
            @table[ix].value
        else
            nil
        end
    end

    def []=(key, value)
        ix = _find(key)
        if ix >= 0
            @table[ix].value = value
        else
            @table.push(KeyValue.new(key, value))
        end
    end

    def delete(key)
        value = nil
        ix = _find(key)
        if ix >= 0
            value = @table[ix].value
            @table.delete_at(ix)
        end
        value
    end
end

def test    # Unit Test
    table = Table.new
    table["Japan"] = "Edo"
    table["US"] = "Washington"
    table["UK"] = "London"
    table["USSR"] = "Moscow"
    puts table
    table["Japan"] = "Tokyo"
    table.delete("USSR")
    puts table

end

test