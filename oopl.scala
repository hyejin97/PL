abstract class Tree[+T]
case class Node[+T](v:T, l:Tree[T], r:Tree[T]) extends Tree[T]
case class Leaf[+T](v:T) extends Tree[T]

trait Addable[T] {
    def +(toadd: T):T
}

class A(x:Int) extends Addable[A]{
    val value = x
    def +(toadd: A) = new A(this.value + toadd.value);
    override def toString(): String = (s"A($x)")
}

class B(x:Int) extends A(x:Int){
    override def toString(): String = (s"B($x)")
}

class C(x:Int) extends B(x:Int){
    override def toString(): String = (s"C($x)")
}

object Part2{
    
    def inOrder[T](tr: Tree[T]):List[T] = {
        tr match{
            case Leaf(v) => List(v)
            case Node(v, l, r) => inOrder(l) ++ List(v) ++ inOrder(r)
                                   
        }
    }
  
    def treeSum[T <: Addable[T]](tr: Tree[T]):T = {
        tr match{
            case Leaf(v) => v 
            case Node(v, l, r) => treeSum(l) + v + treeSum(r)

        }
    
    }

    def treeMap[T, V](f:T=>V, tr:Tree[T]):Tree[V] = {
        tr match{
            case Leaf(v) => Leaf(f(v))
            case Node(v, l, r) => Node(f(v), treeMap(f, l), treeMap(f, r))
        }
    }

    def BTreeMap(f:B=>B, tr:Tree[B]):Tree[B] = {
        tr match{
            case Leaf(v) => Leaf(f(v))
            case Node(v, l, r) => Node(f(v), treeMap(f, l), treeMap(f, r))
        }
    }

    def test():Unit = {
      def faa(a:A):A = new A(a.value+10)
      def fab(a:A):B = new B(a.value+20)
      def fba(b:B):A = new A(b.value+30)
      def fbb(b:B):B = new B(b.value+40)
      def fbc(b:B):C = new C(b.value+50)
      def fcb(c:C):B = new B(c.value+60)
      def fcc(c:C):C = new C(c.value+70)
      def fac(a:A):C = new C(a.value+80)
      def fca(c:C):A = new A(c.value+90)

      val myBTree: Tree[B] = Node(new B(4),Node(new B(2),Leaf(new B(1)),Leaf(new B(3))), 
		               Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))

      val myATree: Tree[A] = myBTree

      println("inOrder = " + inOrder(myATree))
      println("Sum = " + treeSum(myATree))

      //println(BTreeMap(faa,myBTree))
      println(BTreeMap(fab,myBTree))
      //println(BTreeMap(fba,myBTree))
      println(BTreeMap(fbb,myBTree))
      println(BTreeMap(fbc,myBTree))
      //println(BTreeMap(fcb,myBTree))
      //println(BTreeMap(fcc,myBTree))
      println(BTreeMap(fac,myBTree))
      //println(BTreeMap(fca,myBTree))

      println(treeMap(faa,myATree))
      println(treeMap(fab,myATree))
      //println(treeMap(fba,myATree))
      //println(treeMap(fbb,myATree))
      //println(treeMap(fbc,myATree))
      //println(treeMap(fcb,myATree))
      //println(treeMap(fcc,myATree))
      println(treeMap(fac,myATree))
      //println(treeMap(fca,myATree))

    }	
    
    def main(args: Array[String]) = {
      test()
    }
}
