object MyModule { // object creates a singleton type

	private def formatResult(name: String, n: Int, f: Int => Int) = {
		val msg = "The %s of %d is %d"
		msg.format(name, n, f(n))
	}

	// pure function
	def abs(n: Int): Int =
		if (n < 0) -n
		else n

	def factorial(n: Int) = {
		def go(n: Int, acc: Int): Int =
			if (n <= 0) acc
			else go(n - 1, n * acc)

		go(n, 1)
	}

	def fibonacci(n: Int): Int = {
		def go(n: Int, prev: Int = 0, next: Int = 1): Int = n match {
			case 0 => prev
			case 1 => next
			case _ => go(n - 1, next, (next + prev))
		}

		go(n)
	}

	def binarySearch(ds: Array[Double], key: Double): Int = {
		@annotation.tailrec
		def go(low: Int, mid: Int, high: Int): Int = {
			if (low > high) -mid - 1
			else {
				val mid2 = (low + high) / 2
				val d = ds(mid2)
				if (d == key) mid2
				else if (d > key) go(low, mid2, mid2-1)
				else go(mid2 + 1, mid2, high)
			}
		}

		go(0, 0, ds.length - 1)
	}

	def binarySearchGenerics[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
		@annotation.tailrec
		def go(low: Int, mid: Int, high: Int): Int = {
			if (low > high) -mid - 1
			else {
				val mid2 = (low + high) / 2
				val a = as(mid2)
				val greater = gt(a, key)
				if (!greater && !gt(key, a)) mid2
				else if (greater) go(low, mid2, mid2 -  1)
				else go(mid2 + 1, mid2, high)
			}
		}
		go(0, 0, as.length - 1)
	}

	def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
	    @annotation.tailrec
	    def iter(i: Int): Boolean = {
	      if (i >= as.length - 1) true
	      else !gt(as(i), as(i + 1)) && iter(i + 1)
	    }
	    iter(0)
	}


	def main(args: Array[String]): Unit = {
		println(formatResult("absolute value", -42, abs))
		println(formatResult("factorial value", 7, factorial))
		println(formatResult("increment", 7, (x: Int) => x + 1))
		println(formatResult("increment2", 7, (x) => x + 1))
		println(formatResult("increment3", 7, x => x + 1))
		println(formatResult("increment4", 7, _ + 1))
		println(formatResult("increment5", 7, x => { val r = x + 1; r }))		
	}

}