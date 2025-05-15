from collections.abc import Mapping, MutableMapping

class Table(MutableMapping):
    def __init__(self):
        self.__table = []      # the internal table as a list of key-value pairs

    def __len__(self):
        return len(self.__table)

    def __str__(self):
        result = "{\n"
        for kv in self.__table:
            result += "\t" + str(kv[0]) + ": " + str(kv[1]) + ",\n"
        result += "}"
        return result

    def __lin_search(self, key):
        for ix in range(len(self.__table)):
            if self.__table[ix][0] == key:
                return ix
        return -1
    
    def __search(self, key):
        return self.__lin_search(key)

    def __contains__(self, key):
        return self.__search(key) != -1
    
    def __getitem__(self, key):
        for kv in self.__table:
            if kv[0] == key:
                return kv[1]
        return self.__missing__(kv[0])

    def get(self, key, defaultvalue=None):
        for kv in self.__table:
            if kv[0] == key:
                return kv[1]
        return defaultvalue

    def __setitem__(self, key, value):
        for ix in range(len(self.__table)):
            if self.__table[ix][0] == key:
                self.__table[ix] = (key, value)
                return
        else:               # this else corresponds to *for* but not *if*
            self.__table.append((key, value))

    def __delitem__(self, key):
        ix = self.__search(key)
        if ix in range(len(self.__table)):
            self.__table.pop(ix)

    def __iter__(self):
        return self.keys()
    
    def keys(self):
        for kv in self.__table:
            yield kv[0]
    
    def values(self):
        for kv in self.__table:
            yield kv[1]
    
    def items(self):
        for kv in self.__table:
            yield kv

# test routine
def main():
    table = Table()
    table["Japan"] = "Edo"
    table["England"] = "London"
    table["Soviet"] = "Moscow"
    print(table)
    table["Japan"] = "Tokyo"
    del table["Soviet"]
    print(table)
    print("Japan" in table)
    print("Soviet" in table)

if __name__ == "__main__":
    main()
