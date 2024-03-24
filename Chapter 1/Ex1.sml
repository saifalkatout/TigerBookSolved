type key = string
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert(key, LEAF) = TREE(LEAF, key, LEAF)
	| insert(key, TREE(l,k,r)) = 
				if key<k 
					then TREE(insert(key,l),k,r)
				else if key > k
					then TREE(l,k,insert(key,r))
				else TREE(l,key,r)

(* a *)
fun member(key,LEAF) = false
	| member(key, TREE(l,k,r)) = 
				if key = k then true
				else if key > k then member(key, r)
				else member(key, l)
(* b NOT SOLVED *)


(* c *)
(* Not Balanced is easily proved *)


(* d *)
(* Red Black Trees *)