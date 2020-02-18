trait List[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: List[A]

  def ::[U >: A](x: U): List[U] = Construct(x, this)

  def :::[U >: A](prefix: List[U]): List[U] =
    if (prefix.isEmpty) this
    else prefix.head :: prefix.tail ::: this

  def size: Int
  def drop(count: Int): List[A]
  def map[U](f: A => U): List[U]
  def filter(f: A => Boolean): List[A]
}
case class Construct[+A](head: A, tail: List[A]) extends List[A] {
  override def isEmpty: Boolean = false

  def size: Int =
    if(isEmpty) 0
    else 1 + tail.size

  def drop(count: Int): List[A] =
    if (isEmpty) Nil
    else if (count <= 0) this
    else tail.drop(count - 1)

  def map[U](f: A => U): List[U] =
    if (isEmpty) Nil
    else f(head) :: tail.map(f)

  def filter(f: A => Boolean): List[A] =
    if (f(head)) Construct(head, tail.filter(f))
    else tail.filter(f)
}
case object Nil extends List[Nothing] {
  override def isEmpty = true
  override def size: Int = 0
  override def drop(count: Int): List[Nothing] = Nil
  override def map[U](f: Nothing => U): List[U] = Nil
  override def filter(f: Nothing => Boolean): List[Nothing] = Nil
  def head = throw new NoSuchElementException("head of empty list")
  def tail = throw new NoSuchElementException("tail of empty list")
}
object Main extends App {
  val result = Construct(1, Construct(2, Construct(3, Nil)))
  val resultMap = result.map(x => x * 14)
  val resultFilter = result.filter(x => x == 2)
  val resultDrop = result.drop(1)
}