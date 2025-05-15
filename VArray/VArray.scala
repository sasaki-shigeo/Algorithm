import scala.collection.mutable

class VArray[T](initialSize: Int) extends Seq[T] {
    protected var array = new Array[AnyRef](initialSize).asInstanceOf[Array[T]]

    protected var limit: Int = 0

    def this() = this(10)
    
    def capacity = array.length
    def length: Int = limit

    def apply(n: Int): T = array(n)
    def update(n: Int, x: T): Unit = {
        array(n) = x
    }


    def iterator = new Iterator[T] {
        private var ix = 0
        def hasNext: Boolean = (ix < limit)
        def next(): T = {
            val result = array(ix)
            ix += 1
            result
        }
    }

    def +=(x: T) = {
        if (limit >= capacity) {
            val newArray = new Array[AnyRef](2 * limit).asInstanceOf[Array[T]]
            for (i <- 0 until limit) {
                newArray(i) = array(i)
            }
            array = newArray
        }

        array(limit) = x
        limit += 1

        this
    }

    def remove(n: Int): T = {
        val result = array(n)
        limit -= 1
        for (i <- n until limit) {
            array(i) = array(i+1)
        }
        result
    }
}

object VArray {
    def apply[T](xs: T*): VArray[T] = {
        val varray = new VArray[T](xs.length)
        for (x <- xs) {
            varray += x
        }
        varray
    }

    def main(args: Array[String]): Unit = {
        val xs = VArray(1, 2, 4, 8)
        println(xs)
        println(xs.mkString(", "))
        xs += 16
        println(xs)
    }
}