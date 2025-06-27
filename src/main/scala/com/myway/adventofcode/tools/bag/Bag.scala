package com.myway.adventofcode.tools.bag

case class Bag[A](bagMap: Map[A, Long]) {

  def this() = this(Map[A, Long]())

  def this(a: A, l: Long) = this(Map[A, Long](a -> l))

  def this(a: A) = this(a, 1L)

  lazy val total: Long = this.bagMap.values.sum

  def addMany(a: A, l: Long): Bag[A] = Bag(bagMap.get(a) match {
    case None => this.bagMap + (a -> l)
    case Some(l0) => this.bagMap - a + (a -> (l + l0))
  })

  def add(a: A): Bag[A] = this.addMany(a, 1L)

  def get(a: A): Long = this.bagMap.getOrElse(a, 0L)

  def map[B](f: A => B): Bag[B] = flatMap(a => new Bag(f(a)))

  private def mapValues(f: Long => Long): Bag[A] =
    Bag(this.bagMap.toList.map({ case (k, v) => (k, f(v)) }).toMap)

  def merge(that: Bag[A]): Bag[A] = that.bagMap.keySet.foldLeft(this)(
    (bg, k) => bg.addMany(k, that.get(k))
  )

  def flatMap[B](g: A => Bag[B]): Bag[B] =
    this.bagMap.keySet.foldLeft[Bag[B]](new Bag())(
      (bg, a) => bg.merge(g(a).mapValues(_ * this.get(a)))
    )

  def cross[B](that: Bag[B]): Bag[(A, B)] = Bag((for {
    ak <- this.bagMap.toList
    bh <- that.bagMap.toList
  } yield ((ak._1, bh._1), ak._2 * bh._2)).toMap)


}

object BagOps {

  def codomain[K, V](map: Map[K, V]): Bag[V] = map.keySet.foldLeft[Bag[V]](new Bag())(
    (bg, k) => bg.add(map(k))
  )

  def ix[K, V](bag: Bag[(K, V)]): Map[K, Bag[V]] =
    bag.bagMap.keySet.foldLeft[Map[K, Bag[V]]](Map())(
      (mp, kv) => mp +
        (kv._1 ->
          new Bag(kv._2, bag.get(kv)).merge(mp.getOrElse(kv._1, new Bag()))
          )
    )

  def ix0[K, V](map:Map[K, Bag[V]]) : Bag[(K, V)] = map.keySet.foldLeft[Bag[(K, V)]](new Bag())(
    (bg,k) => map.get(k).map(_.map(v=>(k,v))).get.merge(bg)
  )

}