sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def tail[A](ts: List[A]) = ts match {
		case Nil => sys.error("init of empty error")
		case Cons(x, xs) => xs
	}

	def drop[A](ts: List[A], n: Int) = {
		if (n > 0) {
			ts match {
				case Nil => sys.error("init of empty list")
				case Cons(x, xs) => drop(xs, n - 1)
			}
		} else ts
	}

	def dropWhile[A](l: List[A])(predicate: A => Boolean): List[A] = {
		ts match {
			case Nil => sys.error("init of empty list")
			case Cons(head, tail) =>
				if (!predicate(head))
					dropWhile(tail)(predicate)
				else
					ts
		}
	}

	def setHead[A](ts: List[A], head: A): List[A] = ts match {
		case Nil => Cons(head, List())
		case Cons(x, xs) => Cons(head, xs)
	}

	val example = Cons(1, Cons(2, Cons(3, Nil)))
	val example2 = List(1, 2, 3)
	val total = sum(example)
}