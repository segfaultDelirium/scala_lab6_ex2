class A {
  override def toString: String = "A"
}

class B(val x: Int) extends A {
  override def toString: String = s"B: ${x.toString}" // B: "+x,toString
}

class C(x: Int) extends B(x) {
  override def toString: String = s"C: ${x.toString}"
}

// trait ReplaceTrait[-T, R] {
//   def replace(index: Int): T => TwistedMonoPair[R] = {
//     if index == 0 then {
//       return (newValue: T) => TwistedMonoPair(newValue, right)
//     }
//     return (newValue: T) => TwistedMonoPair(left, newValue)
//   }
// }

case class TwistedMonoPair[+T](left: T, right: T){// extends ReplaceTrait[T, T]{
  def apply(index: Int): T = {
    if index == 0 then {
      return left
    }
    if index == 1 then {
      return right
    }
    return left;

  }

  def replace[R>:T](index: Int): R => TwistedMonoPair[R] = {
    if index == 0 then {
      return (newValue: R) => TwistedMonoPair(newValue, right)
    }
    return (newValue: R) => TwistedMonoPair(left, newValue)
  }
}

@main def hello: Unit = {
  val a: TwistedMonoPair[A] = TwistedMonoPair[A](new B(9), new A)
  println(a(0))
  println(a(1))
  println(a)
  val b: TwistedMonoPair[A] = TwistedMonoPair[B](new B(9), new B(77))
  println(b)
  // val c: TwistedMonoPair[A] = new TwistedMonoPair[B](new B(9), new A) // should not compile

  val d1 = b.replace(0)(new A)
  println(d1)
  val tA: TwistedMonoPair[A] = d1;
  // val tB: TwistedMonoPair[B] = d1; // should not compile
  val d2 = b.replace(1)(new A)
  println(d2)
}
