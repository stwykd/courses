
// Take left-most operator
// ... evaluate operands
// ... and apply operator
2*5

// A name is evaluated by replacing it with right hand size
// of its definition
def pi = 3.14159

// Evaluation stops once it results in one value
def radius = 2*5*pi
radius

// Definitions can have parameters
def cir_area(radius: Int): Double = radius*pi

// Functions are evaluated in a similar way
def square(x:Double): Double = x*x

def sumOfSquares(x:Double, y:Double): Double =
  square(x)+square(y)

// CALL-BY-VALUE,Used by default in Scala
// ... evaluate all function args from left to right
// ... replace function (here square(x)), by the rhs of def
// ... and replace parameters by actual arguments
sumOfSquares(3+1+9, square(3))

// Substitution model
// - Evaluation reduces an expression to a value
// - Can express any algorithm
// If expression does not reduce to a value, program gets stuck

// Parameters can be passed by-name by using =>
def square_(x: =>Double, y: =>Double) = x*x

// Conditionals are for expressions, not statements
def abs(x:Double):Double = if(x>0) x else -x

// Definitions can be by-name (def) or by value (val)
def x = 3
// def are evaluated at each use, that refers to square(x)
def def_x = square(x)
// val are evaluated at definition, that refers to 9
val val_x = square(x)

def loop: Double = loop
def def_loop = loop
//val val_loop = loop // Infinite loop in trying to evaluate



// When defining an algorithm, define a function for each step
// ... but we want to hide helper functions, so we put them in sqrt block
def sqrt(x: Double) = {
  def isGoodEnough(guess: Double): Boolean =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double): Double =
    (guess + x / guess) / 2

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

// A block {...} contains definitions and return value as last element
// - They are expressions in Scala, therefore can be used everywhere
// outside definitions can be used however unless shadowed
// - That allows to use definitions without passing them as parameters


// Semicolons are used to write multiple expressions in one line
// ... But a new line can be seen as a new expression
1
+ 3 // Seen as 1; +3

1 +
3 // Compiles as expected



// If you have a function that calls itself as last action, then
// the stack frame for it can be reused. Known as tail recursive
def gcd(a: Int, b:Int): Int= if(b == 0) a else gcd(b, a%b)

// Calls to other functions can be tail-recursive by using @tailrec
// This is only necessary for deep recursive chains!

// Making a function tail recursive...
def fact(n:Int): Int = if (n==0) 1 else n*fact(n-1)

def tail_fact(n:Int): Int = {
  def loop(acc:Int, n:Int): Int = if (n==0) acc else loop(acc*n, n-1)
  loop(1, n) }



// Functions are first-class values, they can be passed as parameters
// and returned as results. Functions using them are called higher-order

// Type A=>B is the type of a function that takes type A and returns type B
def sum(f: Int=>Int, a:Int, b:Int):Int = if(a>b) 0 else f(a)+sum(f, a+1, b)

// To define functions on the go use anonymous functions, (x:A) => x+1
def sumInts(a:Int, b:Int):Int = { sum((x:Int) => x, a, b) }
def sumCubes(a:Int, b:Int):Int = { sum((x:Int) => x*x*x, a, b) }
// fact is recursive and cannot be anonymous
def sumFactorials(a:Int, b:Int) = sum(fact, a, b)

// Tail recursive sum!
def tail_sum(f:Int=>Int, a:Int, b:Int) = {
  def loop(acc:Int, a:Int): Int = if(a>b) acc else loop(f(a)+acc, a+1)
  loop(a, 0) }



// In the functions sumInts, sumCubes ... above parameters repeat
// If we define sum by returning a function ...
def sum(f:Int=>Int): (Int,Int)=>Int = {
  def sumF(a:Int, b:Int):Int = if(a>b) 0 else f(a)+sumF(a+1, b); sumF }

// Remove arguments in sum call
def sumIntsF(a:Int, b:Int):(Int, Int) => Int = sum(x=>x)
def sumCubesF(a:Int, b:Int):(Int, Int) => Int = sum(x=>x*x*x)
def sumFactorialsF(a:Int, b:Int):(Int, Int) => Int = sum(fact)
// ... Can still be applied as before
sumIntsF(1,10); sumCubesF(1,10); sumFactorials(1,10)
// Remove sumInts, sumCubes ... middles by ...
sum((x:Int)=>x)(1, 10); sum((x:Int)=>x*x*x)(1, 10); sum(fact)(1,10)
// Returns a function, equivalent to sumFactorials, which takes 1, 10

// There's a special syntax for functions that return functions
def sum(f:Int=>Int): (Int,Int)=>Int = {
  def sumF(a:Int, b:Int):Int = if(a>b) 0 else f(a)+sumF(a+1, b); sumF }
def sumCurr(f:Int=>Int)(a:Int, b:Int):Int = if (a>b) 0 else f(a)+sumCurr(f)(a+1, b)
// That way you can write sum(cube) and use it later
// The additional parameter is the parameter list for the nested function
// You can now write sum(cube) and pass the parameter list later
def sumCubesC = sumCurr((x:Int)=>x*x*x); sumCubesC(3, 7)

// def f(args1)...(argsn)=E becomes def f(args1)...(argsn-1)=(argsn)=>E
// By repeating it n times, def f(args1=>(args2=>(args3=>(argsn=>E)...)
// This style is called currying
def sumCur(f:Int=>Int)(g:Int=>Int)(h:Int=>Int)
def sf = sumCur(x=>x+x); def sg = sf(x=>x+x); sg(x=>x+x)

// sumCur above is of type (f:Int=>Int)(a:Int,b:Int):Int
// ... function that takes a function taking two Ints and returning Int
// Functional types associate to the right Int=>Int=>Int is Int=>(Int=>Int)



class Rational(x:Int, y:Int) { def numer = x; def denom = y}
// That introduces a new type "Rational" and a constructor for it

// We can define Rational as a pure data type and define operations outside
def addRational(r:Rational, s:Rational):Rational =
  new Rational(r.numer*s.denom + s.numer*r.denom, r.denom*s.denom)

addRational(new Rational(1,2), new Rational(2,3))

// ... Or put these in the class and use dot notation (As methods)
class Rational(x:Int, y:Int) { def numer = x; def denom = y
  def add(that:Rational) =
    new Rational(numer*that.denom+that.numer*denom, denom*that.denom)
  override def toString = numer+"/"+denom
}

// private members can only be accessed from within the class, gcd is private
// as we don t want clients to see it, it's strictly for implementation purposes
class Rational(x:Int, y:Int) { def numer = x; def denom = y
  private def gcd(a:Int, b:Int):Int = {if(a==0) a else gcd(b, a%b)}
  private val g = gcd(x, y)
  def numer = x/g; def denom = y/g
}

// Calling gcd(x,y) on numer and denom could be advantageous if functions numer
// and denom are not called really often, we amortize the addtional cost for gcd

// By making numer and denom val's is advantageous if numer and denom are called
// really often because we do not repeat the computations

// Clients observe the same behaviour in each case, this is called data abstraction

// 'this' represents the object on which the current method is executed
def add(that:Rational) =
  new Rational(this.numer*that.denom+that.numer*this.denom, this.denom*that.denom)
// this.numer same as numer here

// A requirement is a test performed at class initialization
// It enforces a precondition on the caller
class Rational(x:Int,y:Int){ require(y!=0, "") ...} // throws IllegalArgumentE.

// An assertion checks the code of the function itself
val x = sqrt(y); assert(x >= 0) // throws AssertionErrorE.

// Every class implicitly introduces a primary construction, which takes
// class parameters and executes the class body. To add more constructors ...
class Rational(x:Int,y:Int){ def this(x:Int) = this(x, 1)}

// In a class, always normalize variables early to avoid ArithmeticOverflows



// We have define functions by using the model of substitution
// We extend it to classes and objects ...
// new C(e1...en) evaluate arguments just like with a function
// The resulting expression new C(v1...vm) is taken as a value

// Suppose we have class C(x1...xm){ ... def f(y1...yn) = b ... }
// new C(v1...vm).f(w1...wn) is rewritten by using three substitutions
// - Substitute formal parameters yi with actual arguments wi, [w1/y1...wn/yn]
// - Replace formal class parameters with actual arguments [v1/x1...vm/xm]
// - Substitute 'this' with the value of new C(v1...vm) itself
// [w1/y1...wn/yn][v1/x1...vm/xm][new C(v1...vm)/this]b
// More substitutions, but still the same evaluation model

// new Rational(1,2).numer -> [1/x,2/y][][new Rational(1,2)/this]x (rhs of numer)
// new Rational(1,2).less(new Rational(2,3))
// -> [1/x,2/y][new Rational(2,3)/that][new Rational(1,2)/this]
// -> new Rational(1,2).numer*new Rational(2,3).denom < new Rational(2,3).numer*
// ... new Rational(1,2).denom ->* 1*3 < 2*2 ->* true



// Any method parameter cab be used like an infix operator, r add s like r.add(s)
// Identifiers are alphanumeric like Java. They can be symbolic, start with an
// operator symbol followed by operator symbols (eg +?%&)
// Operator symbols can be added to the end of alphanumeric identifiers at the end
// by adding _ before them (eg counter__)



// To implement IntSet we use the binary tree data structure
// - A tree repsenting the empty set
// - tree consisting of an integer and two subtrees
abstract class IntSet {
  def incl(x:Int): IntSet // Body missing, ok for abstract classes
  def contains(x:Int): Boolean }

class Empty extends IntSet{
  def contains(x:Int): Boolean = false
  def incl(x:Int): IntSet = new NonEmpty(x, new Empty, new Empty) }

class NonEmpty(elem:Int, left:IntSet, right:IntSet) extends IntSet{
  def contains(x:Int):Boolean = if(x < elem) left contains x
  else if(x > elem) right contains x else true
  def incl(x:Int):IntSet = if(x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x) else this }
// Persisten data structure, Old versions of it are still maintained

// extends makes the class a subclass of the extended superclass, subclasses
// can be used where the superclass is required
// In Scala every user defined class extends another class. If no explicit
// superclass is given, then Object is assumed

// We can redefine existing, non-abstract definition of a superclass in a
// subclass bt using override
abstract class Base {def foo = 1; def bar:Int}
class Sub extends Base { override def foo = 1; //override mandatory!
  def bar = 3 } // override optional

// object defines a singleton, there is only one, no need to use new,
// will be create the first time it is reference
// Singletons are values, no further evaluation required
object Empty extends(IntSet){...}
new NonEmpty(elem, Empty, Empty)

// Each application contains an object with a main method


abstract class IntSet {... def union(other:IntSet):IntSet ...}
class Empty extends IntSet {
  def union(other:IntSet):IntSet = other }
class NonEmpty extends IntSet { def union(other:IntSet):IntSet =
  ((left union right) union other) incl elem) }
// That has all the parts of the previous one. It contains the left set,
// right set, the elem and the other set
// Every call is with a subset of the starting set, first is on the
// left subtree, then union of left and right subtree without elem
// At some point, the set will become empty, the recursion terminates

// OO languages use dynamic method dispatch model
Empty contains 1 // Look up contains in Empty, performs evaluation
new NonEmpty(7,Empty,Empty) // Look up contains in NonEmpty, ...



// Classes and objects are organized in packages
// To place a class or object in a package add the package name at
// the beginning of your source file, that'll put then every class
// or object defined later, in that package
package progfun.example object Hello {}; class Hellow {}

// Refer to Hello from your program or the comma  nd line by
// using its fully qualified name
> scala progfun.example.Hello

// import works like in Java, few differences ...
import progfun.example.{Hello,Hellow} // import multiple Classes
import progfun.example._ // wildcard import
// You can import from either a package or an object

// Some entities are imported automatically in Scala, those are ...
// all members of package scala, java.lang and scala.Predef

// Explore the standard Scala library at scala-lang.org/api/current

// A class can only have one superclass (like Java)
// A trait is declared like an abstract class
trait Planar { def h:Int, def w:Int; def surf = h*w }
// Classes, objects and traits can inherit from many traits
class Square extends Shape with Planar with Movable
// traits resembles interfaces in Java, however are more powerful
// as they can contain fields and concrete methods
// However traits cannot have value parameters, only classes can

// In the scala's class hierarchy you have scala.Any, superclass
// of every other class, that divides in AnyVal and AnyRef.
// AnyVal are essentially the primitive types you see from Java
// AnyRef is an alias for Object, contains all other classes
// Any defines ==, != (mapped to Java equals), hashCode, toString

// Nothing is the subtype of every scala type
// Scala's has exceptions like Java
throw Exception // Abort evaluation of the program
// Exception has type Nothing

// Null is subtype of all the AnyRef classes and the type of
// the null value
// As in Java, it is the value of every reference type
val x = null // x:Null = null
val y:String = x // y:String = null
val z:Int = x // Not reference type, error!



// a consList, is an immutable list costructed from
// Nil, an empty list, and Cons, cells containing an element
// and the rest of the list
trait List[T] { // Takes type param T, generalizes the
  // definition of List (type params are written in [...])
  def isEmpty:Boolean; def head:T; def tail:List[T] }

// val here defines a field at the same time
class Cons[T](val head:T, val tail:List[T]) extends List[T] {
  def isEmpty = false // We implemented head and tail with val
  // val definitions are special cases of methods, they can
  // override methods and implement abstract methods and traits
}
class Nil[T] extends List[T] { def isEmpty = true
  def head:Nothing = throw new NoSuchElementException("Nil.head")
  def tail:Nothing = throw new NoSuchElementException("Nil.tail") }

// Type parameters can also be applied to functions
def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
singleton[Int](1); singleton(true) // type param is deduced
// Type params do not affectt evaluation in scala, all type params
// are removed before evaluating the program, called type erasure

// Polymorphism, types can have different instances or the
// function can now be applied to arguments of different types
// Generics, instances of a function or classe are created by
// type parameterization (List[Int], List[Double], List[Boolean])
// Subtyping, Instances of a subclass can be passed to a base class
// (where List is needed, pass either Nil or Cons)

def nth[T] (n:Int, xs:List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundException
  else if (n == 0) xs.head else nth(n-1, xs.tail)



// Scala is a pure OO language, every value is an object and
// every operator a method. Type of each value is a class

// Primitive classes are like other class, however values are
// represented by using Java primitive values for effiency reasons
// For example, Boolean
abstract class Boolean {
  def ifThenElse[T](t:=>T, e=>T): T
  def && (x:=>Boolean) = ifThenElse(x, false)
  def || (x:=>Boolean) = ifThenElse(true, x)
  def unary_!: Boolean = ifThenElse(false, true)
  def == (x:Boolean) = ifThenElse(x, x.unary_!)
  def != x:Boolean = ifThenElse(x.unary_!, x) }

object true extends Boolean { def ifThenElse[T](t:=>T,e:=>T)= t }
object true extends Boolean { def ifThenElse[T](t:=>T,e:=>T)= e }

// For implementing integers ....
abstract class Nat {
  def isZero: Boolean; def predecessor: Nat; def successor = new Succ(this)
  def + (that:Nat): Nat; def - (that:Nat): Nat }

object Zero extends Nat {
  def isZero = true; def predecessor = throw new Error("0.predecessor")
  def + (that:Nat) = that
  def - (that:Nat) = if (that.isZero) this else throw new Error("neg") }

class Succ(n: Nat) extends Nat {
  def isZero = false; def predecessor = n
  def + (that:Nat) = new Succ(n+that)
  def - (that:Nat) = if (that.isZero) this else n - that.predecessor s}

// We treat natural numbers from first principles, primitive types aren t needed
// Booleans are used, but as we've seen, also they can be encoed as classes
// Those are called Peano numbers!

// Functions are treated are treated as objects
// type A => B is an abbreviation for the class scala.Function1[A, B]
trait Function1[A,B] { def apply(x:A): B } // roughly defined like that
// There are Function2, Function3 for more parameters
// Functions are just classes with the apply method

(x:Int) => x*x //expanded to...
{ class AnonFun extends Function1[Int, Int] {
  def apply(x:Int) = x*x } new AnonFun }
// That definition is quite common that there's a special syntax for it
new Function1[Int,Int] {def apply(x:Int) = x*x} // Anonymous class syntax

// A function call f(a,b) extpands to f.apply(a, b), OO translation is ...
val f = (x:Int) => x*x; f(7)
val f = new Function1[Int,Int] {def apply(x:Int) = x*x } f.apply(7)

// Functions are objects. But that doesn t work for methods.
// So methods such as def f(x:Int) Boolean are not function values
// But if f is used where a function type is expected, it is converted
// automatically to the function value (x:Int) => f(x) and expanded

object List {
  // List(1, 3) = List.apply(1, 3)
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, new Nil))
  def apply[T]() = new Nil }



// Two principal forms of polymorphism, subtyping where we can pass
// instances of a subtype where a base type was required, and generics
// where we can parameterize types with other types
// We will look at interactions of those concepts, bounds and variance
// Bounds, we can subject type parameters to subtype constraints
// Variance, how parameterized types behave under subtyping

// For bounds, consider the method assertAllPos which takes IntSet
// and returns it if all elements are positive, else throw exception
def assertAllPos(s:IntSet): IntSet
// Works for most cases, but we can be more precise
// To express that assertAllPos takes Empty sets to Empty sets and
// NonEmpty sets to NonEmpty sets we write ...
def assertAllPos[S <: IntSet](r:S): S
// it takes type S, that must be subtype of IntSet, and returns
// a result of that same type
// S <: T means S is a subtype of T, called lower bound
// S >: T means S is a supertype of T, called upper bound
// [S >: NonEmpty] could be NonEmpty, IntSet, AnyRef, or Any
// [S >: NonEmpty <: IntSet] lower and upper bounds can be mixed

// List[NonEmpty] <: List[IntSet], We call types for which this
// relationship holds are called covariant because their
// subtyping varies with the type parameter
// Not all types are covariants. Liskov substitution principle tells
// us when a type can be a subtype of another
// generally it says that if A <: B, then everything one can do with
// a value of type B one should also be able to do with type A



trait Expr { def isNumber: Boolean; def isSum: Boolean
  def numValue:Int; def leftOp:Expr; def rightOp:Expr }

class Number(n:Int) extends Expr { def isNumber: Boolean = true
  def isSum: Boolean = false; def numValue:Int = n
  def leftOp:Expr = throw new Error("Number.leftOp")
  def rightOp:Expr = throw new Error("Number.rightOp") }

class Sum(e1:Expr, e2:Expr) extends Expr {
  def isNumber: Boolean = false; def isSum:Boolean = true
  def numValue: Int = throw new Error("Sum.numValue")
  def leftOp: Expr = e1; def rightOp: Expr = e2 }

def eval(e:Expr): Int = {
  if(e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression" + e) }
// Adding a new class, such as Prod for product, will change
// existing classes and extend methods. This is tedious
// The number of methods that needed grow fast as classes are added

// A "hacky" solution uses type tests and type casts, but Scala
// discourages their use as there are better alternatives
def eval(e:Expr): Int =
  if(e.isInstanceOf[Number]) e.asInstanceOf[Number].numValue
  else if (e.instanceOf[Sum])
  eval(e.asInstanceOf[Sum].leftOp) + eval(e.asInstanceOf[Sum].rightOp)
  else throw new Error("Unknown expression " + e)
// Doing it is potentially unsafe, but it is in Scala to
// increase interoperability with Java

 // A better solution is ...
 trait Expr { def eval:Int }
 class Number(n:Int) extends Expr { def eval: Int = n}
 class Sum(e1:Expr, e2:Expr) extends Expr {
   def eval:Int = e1.eval + e2.eval }
// However, adding to add a "show" method, that displays the Expr
// we'll have to change every subclass

// Say we want to simplify the expressions, this is a non-local
// simplification and cannot be encapsulated in a single object

// We want a general and convenient way to access objects in
// an extensible class hierarchy
// With classification and access methods, creates too many methods
// Types tests and casts, it is unsafe and low-level
// OO decomposition, not always works, need to touch all classes
// to add a new method

// The purpose of test and accessor functions is to reverse the
// construction process. This situation is so fundamental and common
// that Scala automates it with Pattern Matching

// A case class is similar to normal class definition, exception that
// it is preceeded by the modifier case
trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr
// It implicitly defines companion objects containing factory methods
// that construct Numbers, or Sum elements directly
object Number { def apply(n:Int) = new Number(n) }
object Sum { def apply(e1:Expr, e2:Exp) = new Sum(e1, e2) }
// You can write Number(1) instead of new Number(1)
// These classes are now empty, to access components we use
// pattern matching, (can be seen as a generalization of switch)
def eval(e:Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2) }
// Start with a selector expression e
// match keyword is followed by a sequence of cases, pat => expr
// Each case associates an expression expr with a pattern pat
// A MatchError is thrown if no pattern matches
// e match { case pat => expr ... }

// Patterns are constructed from ...
// Constructors, variables, wildcard patterns, constants!
Number(n); Number(_); 1, true, "abc", N
Sum(Number(1), Var(x))
// Patterns can be combined. After the  =>, we can refer to x
// as the name of that variable
// To distinguish a variable n (matches everything) to a costant N
// (matches a value) in scala, variables must be lowercase, and
// constants must be uppercase

// e match { case p1 => e1 ... case pn => en }
// matches the value of the selector e with pattern p1,...pn in the order
// in which they are written
// If a pattern matches the whole expression is rewritten to the rhs of
// the first pattern that matches
// Reference to pattern variables in the pattern are replaced by the
// corresponding parts in the selector

// A constructor pattern C(p1...pn) matches all the values of type C
// (or subtype) that have been constructed with arguments matching the
// pattern p1...pn
// A variable pattern x matches any value, and binds the name of the
// variable x to this value (it can then be in the associated expression)
// A constant pattern c matches any value that is equal to c (like ==)

eval(Sum(Number(1), Number(3))) // Given the matching ...

def eval(e:Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2) } // Evaluates to ...

Sum(Number(1), Number(2)) match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2) // ...
}

eval(Number(1)+Number(3)) // ...

Number(1) match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}+ eval(Number(3)) // ...

1+eval(Number(3)) // Do the same and you get 1 + 3

// It's possible to define to have pattern matching methods inside the
// class hierarchy as methods of base classes themselves
// That's the same of the previous eval method, except that we now
// match on the receiver object itself
trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}
// Both designs are fine. If you're more often creating new
// subclasses of expressions then OO decompisition is best else if
// you're more often creating new methods pattern matching is better
// This is called the expression problem

def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => show(l) + " + " + show(r)
}
// A method for showing the expression, can now be simply added



// List is a fundamental data structure in functional programming
List(1,2,3,4); List(List(1,0,0), List(0,1,0), List(0,0,1)); List()
// Lists are immutable and recursive, while arrays are flat
// Like arrays, elements of a list must all have the same type

// All lists are constructed from empty list, Nil, and the construction
// operation :: (pronounced cons)
// x::xs gives you a new list with first elemnt x, followed by the
// elemets of xs
1::(2::(3::4::Nil)); Nil // Corresponding to List(1,2,3,4) and List()
// By convention, all operators ending in ':' associate to the right
// (All other operators associate to the left)
// A::B::C is interpreted as A::(B::C)
// Operators ending in ':' are seen as method calls of the right operand
Nil.::(4).::(3).::(2).::(1) // Same of above

// Lists have three fundamental operations. isEmpty, head, and tail

// It s also possible (often preferred), to decompose lists with
// pattern matching. The patterns you can apply on lists are the
// same as the construction methods of lists
// Nil matches the Nil costant
// p::ps Matches a list with head p and tail ps
// List(p1...pn) same as p1:: ... :: pn :: Nil
1 :: 2 :: xs // Matches list starting with 1 and then 2, rest
// of the list is arbitrary and bound to the variable xs
x :: Nil // Matches list of length 1, with arbitrary head
List(x) // Same as x::Nil
List() // Same as Nil
List(2 :: xs) // List containing as only element another list
// starting with 2 and with artibtrary tail

// Suppose we want to sort a list, one way would be
// Sort the tail of the list then insert head in the right place
// This is insertion sort
def isort(xs:List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys)) }

def insert(x:Int, xs:List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x<=y) x::xs else y::insert(x, ys) }



// More efficient than insertion sort
// A good example is mrege sort, go like that ...
// If the list has zero elements, already sorted
// Otherwise ...
// Separate list into two sublists of half the original size
// Sort the two sublists
// Merge into a single sorted list
def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if(n==0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
      xs match {
        case Nil => ys
        case x :: xs1 =>
          ys match {
            case Nil => xs
            case y :: ys1 =>
              if(x < y) x :: merge(xs1, ys)
              else y :: merge(xs, ys1)
          }
      }

    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}
// The splitAt function on lists returns two sublists containing
// elements up to the given index and elements from that index
// Those lists are returned in a pair, written (x, y)
val pair = ("answer", 42) // Pair of type (String, Int)
// Pairs can be used as patterns
val (label, value) = pair // match 'pair' to label and value
// Works analogously for tuples with more than two elements

// A tuple type (T1,...Tn) is an abbreviation of the
// parameterized type scala.Tuplen[T1,...Tn]
// A tuple expression (e1,...en) is equivalent to the function
// application scala.Tuplen(e1,...en)
// A tuple pattern (p1,...pn) is equivalent to the
// constructor pattern scala.Tuplen(p1,...pn)
// All Tuplen classes are modeled after the following pattern
case class Tuple2[T1, T2](_1: +T1, _2: +T2) {
  override def toString = "(" + _1 + ',' + _2 + ")" }
// The fields of a tuple can be accessed with name _1, _2, but
// pattern matching form is preferred

// A better merge with pairs would be ...
def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
  case (Nil, ys) => ys
  case (xs, Nil) => xs
  case (x :: xs1, y :: ys1) => if (x<y) x::merge(xs1,ys)
    else y::merge(xs, ys1) }



// Mergesort can be only applied to list of Ints
// We can parameterize merge sort
def msort[T](xs: List[T]): List[T] = ...
// but comparison functions might not be defined in that type

// Parameterize merge with the necessary comparison function
def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T]
msort(List("a","b","c")((x:String, y:String) => x.compareTo(y) < 0)
// That ll give the same result as before
msort(List("a","b","c")((x, y) => x.compareTo(y) < 0)
// We can leaves the types out and the scala compiler will infer them
// If you have a function value as parameter, it's preferred to put
// it last so that you have a better chance that types are already inferred

// There is a class in the standard library for ordering
scala.math.Ordering[T] // We can now do ...
def msort[T](xs: List[T])(ord: Ordering[T]) =
  ...
  def merge(xs: List[T], ys:List[T]) =
    ... if (ord.lt(x, y))
  ... merge(msort(fst)(ord), msort(snd)(ord))
msort(List("a","b","c"))(Ordering.String)

// Passing around lt or ord is cumbersome. By making ord an implicit
// parameter, the compiler will figure out the right implicit to pass
// based on the demanded type
def msort[T](xs: List[T])(implicit ord: Ordering[T]) =
  ... merge(msort(fst), msort(snd))
msort(List("a","b","c"))
// I can now leave out the actual parameter in a call and the compiler
// will synthesize one for me

// When we write an implicit parameter and don t write an argument
// that matches it, the compiler will figure out the right implicit
// to pass, based on the demanded type

// Say, a function takes an implicit parameter of type T
// The compiler will search an implicit defintion that ...
// Is marked implicit, has a type compatible with T, and is visible
// at the point of then function call or defined in a companion object
// associated with T
// If the compiler finds a single definition, it will be taken as
// actual argument for the implicit parameter, otherwise error.



// Functions on lists often have very similar structure

// A common operation is to transform each list element and return
// the list of results
def scaleList(xs: List[Double], factor: Double): List[Double] =
  xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)
  } // Multiply each element by the same factor

// This scheme can be generalized to the method 'map' of the List class
abstract class List[T] { ...
  def map[U](f: T => U): List[U] = this match {
    case Nil => this
    case x :: xs => f(x) :: xs.map(f)
  }
}

def scaleList(xs: List[Doible], factor: Double) =
  xs map (x => x * factor)

// Another example ...
def squareList(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => this
      case y :: ys => y * y :: squareList(ys)
    }
}

def squareList(xs: List[Int]): List[Int] = {
  xs map (x => x*x)
}

// Another common operation on lists is selecting all elements
// satisfying a given condition
def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y>0) y :: posElems(ys) else posElems(ys)
}

// This pattern can be generalized by a method filter
abstract class List[T] {
  ...
  def filter(p: T => Boolean): List[T] = this match {
    case Nil => this
    case x :: xs => if(p(x)) x :: xs.filter(p) else xs.filter(p)
  }
}

def posElems(xs: List[Int]): List[Int] = xs filter (x => x > 0)

// Besides filter, there are other methods that extract sublists
// base on some predicate
val nums = List(2, -4, 5, 7, -1, 3)
nums filter (x => x > 0) // List for which predicate returns true
nums filterNot (x => x > 0)
nums partition (x => x > 0) // filter and filterNot as a pair of lists

// Those other functions look at a prefix and suffix of a list
nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0) // Combines takeWhile and dropWhile

// Function pack that packs consecutive duplicates of list elements
// into sublists
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

// Using pack, write a function encode
def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

// Another common operation on lists is to combine the elements of
// a list using a given operator
// sum(List(x1,...xn)) = 0+x1+...xn
// product(List(x1,...xn)) = 1*x1*...xn
// We can implement this with the usual recursive schema
def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y::ys => y + sum(ys)
}
// This pattern can be abstracted using the generic method reduceLeft
// reduceLeft inserts a given binary operator between adjacent
// elements of a list
// List(x1,...xn) reduceLeft op = (...(x1 op x2) op ...) op n
def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x+y)
def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x*y)
// Instead of ((x, y) => x * y) you can also write (_ * _)
// Every _ represents a function parameter, going from left to right
// By writing (_ * _) the compiler knows that there are two parameters
// say (x * y), so it adds those parameters that it has just
// synthesized, like ((x, y) => ((x * y))
// reduceLeft can only be applied to non empty lists

// foldLeft can also be applied to empty lists
// It takes an operation and an accumulator z as an additional parameter,
// which is retuerned if the list is empty
// (List(x1,...xn)) foldLeft z)(op) = (...(z op x1) op ...) op n
def sum(xs: List[Int]) = (xs foldLeft 0)(_ + _)
def product(xs: List[Int]) = (xs foldLeft 1)(_ * _)

// foldLeft and reduceLeft can be implements in class List as follows
abstract class List[T] {
  def reduceLeft(op: (T, T) => T): T = this match {
    case Nil => throw new Error("Nil.reduceLeft")
    case x :: xs => (xs foldLeft x)(op)
  }
  def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
    case Nil => z
    case x :: xs => (xs foldLeft op(z, x))(op)
  }
}

// foldleft and reduceleft unfold on trees that lean to the left
// There are other operations that unfold trees tgat lean to the right
// called foldRight and reduceRight
// List(x1,...xn-1, xn) reduceRight op = x1 op (...(xn-1 op xn)...)
// (List(x1,...xn) foldRight acc)(op) = x1 op (...(xn op acc)...)

// possible implementations
abstract class List[T] {
  def reduceRight(op: (T, T) => T): T = this match {
    case Nil => throw new Error("Nil.reduceRight")
    case x :: Nil => x
    case x :: xs => op(x, xs.reduceRight(op))
  }
  def foldRight[U](z: U)(op: (T, U) => U): U = this match {
    case Nil => z
    case x :: xs => op(x, (xs foldRight z)(op))
  }
}

// For operators that are associative and commutative, foldleft and
// foldRight are equivalent (there may be difference in efficency)
// But sometimes, only one of the operators is appropriate
def concat[T](xs: List[T], ys List[T]): List[T] =
  (xs foldRight ys)(_ :: _)

(xs foldRight ys)(_ :: _) // That will not work
// Here with foldLeft, we do a foldLeft over a list xs. So we apply the
// operation to each element of the list which is of type T and we get
// the error that cons cannot be applied to arbitrary type elements



// We're going to see other collections, all of these are immutable
// We've seen that lists are linear, meaning access to the first element
// is much faster than access to the middle of the end of the list
// The scala library also defines an alternative sequence implementation
// called Vector, which has a more evenly balanced access pattern than List

// They are usually represented as shallow trees
// A vector up to 32 elements is just an array
//If the vector becomes larger than 32 elements, its representation changes
// to a vector of 32 pointers, each pointing to another array of 32 elements
// This change in implementation is repeated as new arrays become full

// To get an index of a vector you need depth-of-the-vecotr accesses
// Vectors are good for bulk operations, you can do operations in chunks
// of 32 which happens to be close to the size of a cache line,
// meaning that all 32 elements will be in a single cache line, making
// access to each element fairly fast
// For lists there's no guarantee that cons cells are near to each other
// If your operations fit nicely into the model that you take the head
//  of the recursive data structure, and then to take the tail to process
// the rest. If your access pattern have the recursive strucutre, lists
// are better. For bulk operations such as map, fold or filter, a vector
// would be preferable

// It's easy to change between vectors and lists, they're quite analogous
val nums = Vector(1, 2, 3, -88)
val people = Vector("Bob", "James", "Peter")
// They support all list operations, except ::, instead use ...
x +: xs // Create  anew vector with leading element x, followed by
// all elements of xs
xs :+ x // Create new vector with trailing element x, preceeded by
// all elements of xs

// When appending a new element, I need to create a new data structure
// as vectors, like lists, are immutable. So I would take the last
// array (the right-most), and create a new one with the given element
// (If the vector is full I would create a new level here). To combine
// the new array with the initial vector, I can't change the pointer
// as it is immutable so I instead create another copy that points to
// the new element, and the other elements the previous copy pointed to,
// and repeat that up to the root

// Vector and List are two implementations of a concept of sequence,
// represented as a base class Seq of both collections
// Besides Seq we have Sets and Map, all have Iterable as base class.

// There are Arrays and String that look like a sequence. Both support
// the same operations as sequences and can be implicitly converted
// to sequences where needed, but they really come from Java
val xs: Array[Int] = Array(1, 2, 3); xs map (x => 2 *x)
val s: String = "Hello world!"; s filter (_.isUpper)

// Another simple sequence is Range, it represents a sequence of evenly
// spaced integers. There are three operations to, until, by
val r: Range = 1 to 5 // 1, 2, 3, 4, 5
val s: Range = 1 until 5 // 1, 2, 3, 4
1 to 10 by 3 // 1, 4, 7, 10
6 to 1 by -2 // 6, 4, 2

// Ranges are represented as single objects with lower bound,
// upper bound, and step value. These are stored as fields

// Some more operations on sequences ...
xs exists p // true if there is an element x in xs such that p(x) holds
xs forall p // true if p holds for all elements in xs
xs zip yp // takes two sequences and returns a sequence of pairs of
// corresponding elements of the two sequences

val s = "Hello World"
val pairs =List(1,2,3) zip s
// pairs: List[(Int, Char)] = List((1,H), (2,E), (3,L))
pairs.unzip // res0: (List[Int], List[Char]) = (List(1,2,3),List(H,e,l))

xs flatMap f // Takes a collection xs and a function f that maps each
// element of xs to a collection by itself, and concatenates the results
s flatMap (c => List('.', c)) // res0: String .H.e.l.l.o. .W.o.r.l.d

xs sum / xs product // The sum/product of all elements of xs
xs max / xs min // The maximum/minimum of all elements of xs (needs Ordering)

// To list all combinations of numbers x and y where x is drawn from 1..M
// and y is drawn from 1..N
(1 to M) flatMap (x => (1 to N) map (y => (x, y)))

// To compute the scalar product of two vectors
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum
// An alternative way is with pattern matching
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ case(x, y) => x * y }.sum
// Generally { case p1 => ... case pn => en } is shorthand for ...
// x => x match { case p1 => e1 ... case pn => en }

// We can write a prime function like ...
def isPrime(n: Int): Boolean = (2 until n) forall ( d => n % d != 0)



// Higher-order functions on collections often replace loops in
// imperative languages
// Programs that use many nested loops can be expressed often with
// combinations of these higher-order functions

// Find all pairs of positive integers i and j such as j < i < n
// where n is some positive integer and i+j is prime
// A functional way to do this is to ...
// Generate a sequence of all pairs of integers (i, j) such
// that 1 <= j < i < n
// Filter the pairs for which i + j is prime
// To generate the sequence of pairs
// Generate all the integers i between 1 and n (excluded)
// For each integer i, generate the list of pairs (i, 1)...(i, i-1)

val n = 7
(1 until n) map (i =>
  (1 until j) map (j => (i, j)))
// res0: IndexSeq[IndexSeq[(Int, Int)]] = Vector(Vector(), Vector(2, 1),
// ... Vector((3,1), (3,2)), Vector((4,1), (4,2), (4,3)) ... )
// Range is subtype of Seq, (1 until n) got trasformed with a call to map,
// which with the other nested call to map produced a sequence of pairs.
// That cannot be represented with Range, we need some other representation.
// We got an IndexSeq, that sits between Seq and Range.
// IndexSeq uses random access and it is usually represented as a Vector

// The previous step gave a sequence of sequences, let's call it xss
// We can combine subsequences using foldRight with ++
(xss foldRight Seq[Int]())(_ ++ _) // or equivalently ...
xss.flatten // This gives ...
((1 until n) map (i =>
  (1 until j) map (j => (i, j)))).flatten

xs flatMap f // Is really equal to ...
(xs map f).flatten

// So we contract the two to use a flatMap ...
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j)))
// We get a flat sequence of pairs

// We have left to filter such that the sum is a prime
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter ( pair =>
    isPrime(pair._1 + pair._2))

// Higher order functions such as map, flatMap, filter provide powerful
// constructs for manipulating lists
// But sometimes the level of abstraction provided by these functions
// makes the program hard to understand
// Scala's for expression notation can help

case class Person(name: String, age:Int)
// Obtain the names of persons over 20 years ...
for ( p <- persons if p.age > 0) yield p.name // equivalent to
persons filter (p => p.age > 20) map (p => p.name)
// This for-expression is similar to loops in imperative languages, except
// that it builds a list of the results of all iterations. Each element of
// the result is produce by the yield

// A for expression is of the form, for (s) yield e
// Where s is a sequence of generators and filters and e
// is an expression whose value is returned by an iteration
// A generator is of the form p <- e where p is a pattern (also a variable)
// and e is an expression whose value is a collection
// A filter is of the form if f where f is a boolean expression. The filter
// will remove from consideration all elements where f is false
// The sequence must always start with a generator
// If there are several generators in the sequence, the last generators
// vary faster than the first

// Instead of ( s ), braces { s } can be also used, and then the sequence
// of generators and fitlers can be written on multiple lines without
// requiring semicolons

 // Here are two examples that we previously solved with Higher-order
 // functions
 for {
   i <- 1 until n
   j <- 1 until i
   if isPrime(i + j)
 } yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ((x, y) <- xs zip ys) yield x*y).sum



// Sets are very close to sequences. You form sets like sequences ...
val names = Set("Peter", "Jo", "Jack"); val s = (1 to 6).toSet
// Most operations available on sequences are also available on sets
s.map(_ + 2); num filter (_.startsWith == ("Ja")); s.nonEmpty
// Those operations are from Iterable

// The principal difference between sets and sequences are ...
// Sets are unordered, do not have dupliciates and the fundamental operation
// on sets is contains

// N-queens problem, place N queens in a N by N chessboard so that no queen
// is threatened by another

// We can solve this problem with a recursive algorithm
def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  placeQueens(n)

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }
}

// Another fundamental collection type is the map
// A map is of type Map[Key, Value], it associates keys of type Key with
// values of type value
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")
// This is of type Map[String, String]

// Maps are iterables, but are also functions because the
//Class Map[Key, Value] extends the function type Key => Value, so it can
// be used everywhere functions can
// In plarticular, maps can be applied to key arguments
capitalOfCountry("US") // Washington
capitalOfCountry("Andorra") // NoSuchElementException
// To query a map without knowing whether it contains a given key ...
capitalOfCountry get "andorra" // Option[String] = None
capitalOfCountry get "US" // Option[String] = Some(Washington)

// Option is another standard type in the scala library, defined as ...
trait Option[+A]
case class Some[+A](value: A) extends Option[A]
object None extends Option[Nothing]
// The option value can be None, nothing was found or could be Some(x)
// where x is of type A, that means something was found and it is the x

// Since options are defined as case classes, they can be decomposed
// using pattern matching
def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}
// Options also support quite a few operations of other collections
// such as map, flatMap, filter ...

// We need two more operations on collections, groupBy and orderBy
// orderBy orders the elements of a collection according to some criterion
val names = Set("Peter", "Jo", "Jack");
names fortWith (_.length < _.length) // Sort by length
names sorted // Sort with natural ordering

names groupBy(_.head) // Map(J -> (Jo, Jack), P -> (Peter))
// partitions a collection in a map of collections according to some
// discriminative function

// A polynomical can be seen as a map from exponents to coefficients
class Poly(val terms: Map[Int, Double]) {
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
    }
  }
}
val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3-_> 7.0))
p1 + p2

// So far, maps were always partial functions, meaning applying a map to
// a key value in map(key) could lead to an exception, if the key was
// not stored in the map
// There is an operation withDefaultValue that turns a map into a
// total function
val cap1 = capitalOfCountry withDefaultValue "<unknown>"
cap1("Andorra") // <unknown>

// With default values we can make Poly simpler
class Poly(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
}

// It would be nicer for Poly to take the list of values without Map
class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  // The above is a sequence, * means is repeated parameter
  ...
}

// Another way to define + is with foldLeft
class Poly(terms0: Map[Int, Double]) {
  def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  // The above is a sequence, * means is repeated parameter
  ...
}
// foldLeft is more efficient because each of these binding will be immediately
// added to our terms Map, so we build the result directly
