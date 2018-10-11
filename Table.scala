import scala.collection.mutable

class Table[K, V] extends mutable.Map[K, V] {
  protected val table = new mutable.ArrayBuffer[(K,V)]

  private def lin_search(key: K): Int = {
    var (ix, result) = (0, -1)
    while (ix < table.size &&
           { if (key == table(ix)._1) { result = ix; false } else true })
      ix += 1
    result
  }

  protected def index(key: K): Int = lin_search(key)

  def get(key: K): Option[V] = {
    val ix = index(key)
    if (ix == -1)
      None
    else
      Some(table(ix)._2)
  }

  def +=(kv: (K, V)) = {
    val (key, value) = kv
    val ix = index(key)
    if (ix == -1)
      table += kv
    else
      table(ix) = kv

    this
  }

  def -=(key: K) = {
    val ix = index(key)
    if (ix >= 0)
      table.remove(ix)

    this
  }

  def iterator = table.iterator

  override def size: Int = table.size
  override def clear { table.clear }
}
