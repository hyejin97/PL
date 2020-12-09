Control.Print.printDepth := 100;
Control.Print.printLength := 100;

- (*prob 1*)
- fun foo g h x = [h [g x]];        

- (*prob 2*)
- fun bar x [] = []
=  |  bar x (h::t) = (x * h) :: bar x t;

- (*prob 3*)
- fun part x L =   
=     let fun p (L1, L2, []) = (L1, L2)
= 	   |  p (L1, L2, (z::zs)) = if z < x then p (L1 @ [z], L2, zs) else p (L1, L2 @ [z], zs)
=     in
= 	  p ([], [], L)
=     end;

- (*prob 4*)
- fun partSort [] = [] 
=  |  partSort [z] = [z]     
=  |  partSort (x::xs) =
=     let val (L1, L2) = part x xs
=     in
=         partSort L1 @ [x] @ partSort L2
=     end;

- (*prob 5*)
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

- (*prob 6*)
- exception reduce_error
= fun reduce f [x1] = x1
=  |  reduce f [] = raise reduce_error    
=  |  reduce f (x::xs) = f x (reduce f xs);

- (*prob 7*)
- datatype 'a tree = leaf of 'a | node of 'a tree list;

- (*prob 8*)
- fun fringe (leaf L) = [L]
=  |  fringe (node T) = reduce (fn L1 => fn L2 => L1@L2) (map fringe T);

- (*prob 9*)
-  fun sortTree (op <) (leaf L) = (leaf (pSort (op <) L))
=   |  sortTree (op <) (node T) = (node (map (sortTree (op <)) T));

- (*prob 10*)
- fun powerSet [] = [[]]
=  |  powerSet (x::xs) =
= 	let val subset = powerSet xs
= 	in
= 	    map (fn L => x::L) subset @ subset
= 	end;