import scala.collection.mutable

class HashTable[K, V](n: Int) extends mutable.AbstractMap[K, V] {
    protected var table = mutable.ArraySeq.fill(n)(Nil:List[(K, V)])

    protected var count = 0

    def this() = this(10)

    override def size: Int = count
    def capacity: Int = table.size

    override def clear(): Unit = {
        for (i <- 0 until capacity) {
            table(i) = Nil
        }
        count = 0
    }

    def iterator = new Iterator[(K, V)] {
        private var ix = 0

        while (ix < table.size && table(ix).isEmpty) {
            ix += 1
        }

        private var p: List[(K, V)] =
            if (ix < table.size) table(ix) else Nil

        def hasNext: Boolean = ! p.isEmpty

        private def findNextBin: Unit = {
            ix += 1
            if (ix >= table.size)
                p = Nil
            else if (table(ix).isEmpty)
                findNextBin
            else
                p = table(ix)
        }

        def next(): (K, V) = {
            val result = p.head
            p = p.tail
            if (p.isEmpty) {
                findNextBin
            }
            result
        }
    }

    private def hash(key: K): Int =
        (key.hashCode & 0x7FFFFFFF) % table.size

    def get(key: K): Option[V] = {
        for {
            (k, v) <- table(hash(key)).find(_._1 == key)
        } yield v
    }

    def addOne(kv: (K, V)) = {
        def replaceOrAdd(alist: List[(K, V)]): List[(K, V)] = alist match {
            case Nil => { count += 1; List(kv) }
            case (k, v)::xs if k == kv._1 => kv::xs
            case x::xs => x::replaceOrAdd(xs)
        }

        val ix = hash(kv._1)
        table(ix) = replaceOrAdd(table(ix))
        this
    }


    def subtractOne(key: K) = {
        def removeIfExists(alist: List[(K, V)]): List[(K, V)] = alist match {
            case Nil => Nil
            case (k, v)::xs if k == key => { count -= 1; xs }
            case x::xs => x::removeIfExists(xs)
        }

        val ix = hash(key)
        table(ix) = removeIfExists(table(ix))
        this
    }

    private def rehash(): Unit = {
        val backup = table
        table = mutable.ArraySeq.fill(2 * capacity)(Nil:List[(K, V)])
        for {
            slot <- backup
            kv <- slot
        } addOne(kv);
    }
}

object HashTable {
    // test code
    def main(args: Array[String]): Unit = {
        val table = new HashTable[String, String]
        println(table)
        table += "Japan" -> "Tokyo"
        println(table)
        table ++= Seq("US"->"Washington", "France"->"Paris", "UK"->"London",  "Itary"->"Rome")
        println(table)
        table ++= Seq("Germany"->"Berlin", "Holand"->"Amsterdom")
        println(table)
        table.rehash()
        println(table)
    }
}
