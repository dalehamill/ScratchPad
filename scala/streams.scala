/** tests and examples for streams */
object Main {
  def main(args: Array[String]) {
		testConstruction()
		println("all passed")
  }
	def c(name : String) = println(name + " complete")
	
	def testConstruction() = {
		def count(n : Int, by : Int) : Stream[Int] = Stream.cons(n, count(n+by, by))
		assert(15 == count(3,4)(3), "start at 3, count by 4, 4th value should be 15")
		
		// fibonacci 
		def fib(a : Int, b : Int) : Stream[Int] = Stream.cons(a, fib(b, a+b))
		val fibs : Stream[Int] = fib(1,1)
		assert(fibs.take(7).toList == List(1,1,2,3,5,8,13))
		
		// calculating primes (http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
		def sieve(stream : Stream[Int]) : Stream[Int] = Stream.cons(stream.head, sieve(stream.tail filter {_ % stream.head != 0}))
		val primes = sieve(2 #:: count(3,2))
		assert(primes.take(5).toList == List(2,3,5,7,11))
		
		c("testConstruction")
	}
}