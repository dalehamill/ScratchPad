/** tests and examples for pattern matching */
object Main {
  def main(args: Array[String]) {
		testIsFalse()
		testIsKnownArg()
		testProcessArg()
		testEval()
		
		println("all passed")
  }
	def c(name : String) = println(name + " complete")
	
	// flip the specified boolean
	def isFalse(b : Boolean) = b match {
  	case true => false
  	case false => true
	}
	def testIsFalse() = {
		assert(isFalse(false))
		assert(!isFalse(true))
		c("testIsFalse")
	}
	
	// return true if it's a known argument, false otherwise
	def isKnownArg(arg : String) = {
		def help() = true; // always true, but tests calling a function from pattern match
		def unknown(str : String) = "unknown" == str; // only true if unknown was called with 'unknown'
		arg match {
			case "-a" => true
			case "-?" => help()
			case whatever => unknown(whatever)
		}
	}
	def testIsKnownArg() = {
		assert(isKnownArg("-a"), "-a is always known")
		assert(isKnownArg("-?"), "-? calls help(), which is always true")
		assert(isKnownArg("unknown"), "default case, calls unknown(s) which is true for 'unknown'")
		assert(!isKnownArg("blah"), "default case, calls unknown which is false for anything other than 'unknown'")
		c("testIsKnownArg")
	}
	
	def processArg(arg : String, value : Any) = {
		def help() = true; // always true, but tests calling a function from pattern match
		def processName(s : String) = s.length() < 10 // max value on string length to test calling with valid/invalid names
		(arg, value) match {
			case ("-n" | "-name", s : String) => processName(s)
			case ("-count", i : Int) => true
			case ("-?", null) => help()
			case _ => false
		}
	}
	def testProcessArg() {
		assert(processArg("-n", "true"), "name specified '-n' with valid length")
		assert(processArg("-name", "true"), "name specified '-name' with valid length")
		assert(!processArg("-n", "thisshouldbefalse!!"), "name specified with invalid length")
		assert(!processArg("-n", 2), "name called with invalid param (expect to drop to default == false)")
		assert(processArg("-count", 3), "count called with int")
		assert(processArg("-?", null), "help gets called here, second tuple value specified null")
		assert(!processArg("-?", "value"), "help gets called with a String value")
		assert(!processArg("garbage", "can"), "garbage in, expect drop to default false")
		c("testProcessArg")
	}
	
	/*****
	** case classes
	*****/
	sealed abstract class Expression
	case class X() extends Expression
	case class Const(value : Int) extends Expression
	case class Add(left : Expression, right : Expression) extends Expression
	case class Subtract(left : Expression, right : Expression) extends Expression
	
	def eval(expr : Expression, x : Int) : Int = {
		expr match {
			case X() => x
			case Const(n) => n
			case Add(left, right) => eval(left, x) + eval(right, x)
			case Subtract(left, right) => eval(left, x) - eval(right, x)
		}
	}
	def testEval() {
		assert(eval(Add(X(), Const(2)), 3) == 5)
		assert(eval(Add(Subtract(Const(3), X()), Const(3)), 4) == 2)
		c("testEval")
	}
}