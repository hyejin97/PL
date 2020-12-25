Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(*takes a list L of integers and returns a list each of whose elements result
* from multiplying the corresponding element of L by x.*)
- fun bar x [] = []
=  |  bar x (h::t) = (x * h) :: bar x t;

- (*takes an integer x and an integer list L as parameters and partitions L based on x.*)
- fun part x L =   
=     let fun p (L1, L2, []) = (L1, L2)
= 	   |  p (L1, L2, (z::zs)) = if z < x then p (L1 @ [z], L2, zs) else p (L1, L2 @ [z], zs)
=     in
= 	  p ([], [], L)
=     end;

- (*sort a list L of integers, returning a sorted list of the elements of L*)
- fun partSort [] = [] 
=  |  partSort [z] = [z]     
=  |  partSort (x::xs) =
=     let val (L1, L2) = part x xs
=     in
=         partSort L1 @ [x] @ partSort L2
=     end;

- (*A polymorphic partition sort function that will sort a list of elements of any type*)
-  fun pSort (op <) [] = []
=   |  pSort (op <) [x] = [x]
=   |  pSort (op <) (x::xs) =
=     let fun helper (L1, L2, []) = (L1, L2)
=          |  helper (L1, L2, z::zs) =
= 		if z < x then helper (z::L1, L2, zs) 
= 		else helper (L1, z::L2, zs)
= 	  val (L1, L2) = helper([], [], xs)
=     in
= 	  pSort (op <) L1 @ [x] @ pSort (op <) L2
=     end;

- (*takes a function f and a list L, such that if L is of the form
[x1, x2, ..., xn−1, xn], then reduce returns the result of f x1 (f x2 ... (f
xn−1 xn)...)*)
- exception reduce_error
= fun reduce f [x1] = x1
=  |  reduce f [] = raise reduce_error    
=  |  reduce f (x::xs) = f x (reduce f xs);


- datatype 'a tree = leaf of 'a | node of 'a tree list;

- (*The fringe of a tree is a list of the values at the leaves of a tree.*)
- fun fringe (leaf L) = [L]
=  |  fringe (node T) = reduce (fn L1 => fn L2 => L1@L2) (map fringe T);

- (*t is of type ’a list tree. Function sortTree returns a tree with the same structure as t*)
-  fun sortTree (op <) (leaf L) = (leaf (pSort (op <) L))
=   |  sortTree (op <) (node T) = (node (map (sortTree (op <)) T));

- (*the function powerSet L, where L is of type ’a list, returns the power set of L*)
- fun powerSet [] = [[]]
=  |  powerSet (x::xs) =
= 	let val subset = powerSet xs
= 	in
= 	    map (fn L => x::L) subset @ subset
= 	end;
