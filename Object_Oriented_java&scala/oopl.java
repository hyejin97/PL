import java.util.*;

//Define a generic class SortedList<> that implements both the List and Comparable interfaces, 
//such that two objects of type SortedList<> can be compared using the compareTo method.
class SortedList<T extends Comparable<? super T>> extends ArrayList implements Comparable<SortedList<T>>{
	public ArrayList<T> l;
	
	SortedList(){
		l = new ArrayList<T>();
	}

	@Override
	public int compareTo(SortedList<T> o){
        	int k1 = this.l.size();
                int k2 = o.l.size();
		for(int i = 0; i < k2; i++){
                        if(i < k1){
			    int comp = (this.l.get(i)).compareTo(o.l.get(i));
			    if(comp == 0) continue;
                            else if(comp < 0) return -1;
                            else return 1; //comp > 0
                        }
                }
                if(k1 < k2) return -1; // l is shorter than o
                else if(k1 == k2) return 0; // l equals to o
		else return 1; // l is longer than o
        }

	 //add : any new element added to a SortedList is added to the right place in the list in order to keep the SortedList in sorted order
	public boolean add(T e){
		int len = l.size();
		for(int i = 0; i < len; i++){
			if(e.compareTo(l.get(i)) <= 0){
				l.add(i, e);
				return true;
			}
		}
		l.add(len, e);
		return true;
	}

	@Override
	public String toString(){
		String slist = "[";
		for(T e : l) slist = slist + e + " ";
		slist = slist + "]";
		return slist;
	}

}
class A implements Comparable<A>{
	public int a;
	A(int x){
		a = x;
	}
	@Override
	public int compareTo(A e){
		int comp = this.a - e.a;
		if(comp == 0) return 0;
		else if(comp < 0) return -1;
		else return 1;
	}
	@Override
	public String toString(){
		return "A<" + a + ">";
	}

}

class B extends A{
	public int b1, b2;
	B(int x, int y){
		super(x+y);
		b1 = x;
		b2 = y;
	}
	@Override
	public String toString(){
		return "B<" + b1 + "," + b2 + ">";
	}


}

class Part1{
	public static <T> void addToSortedList(SortedList <? super T> L, T z){
		L.add(z);
	}

	public static void test() {
	
		SortedList<A> c1 = new SortedList<A>();
		SortedList<A> c2 = new SortedList<A>();
		for(int i = 35; i >= 0; i-=5) {
	    		addToSortedList(c1, new A(i));
	    		addToSortedList(c2, new B(i+2,i+3));
		}

		System.out.print("c1: ");
		System.out.println(c1);

		System.out.print("c2: ");
		System.out.println(c2);

		switch (c1.compareTo(c2)) {
		case -1:
	    		System.out.println("c1 < c2");
	    	break;
		case 0:
	    		System.out.println("c1 = c2");
	    	break;
		case 1:
	    		System.out.println("c1 > c2");
	    	break;
		default:
	    		System.out.println("Uh Oh");
	    	break;
		}
		
	}

	public static void main(String[] args){
		test();	
	}
}
