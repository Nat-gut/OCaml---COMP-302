(* Question 1: Tree Depth *)
(* TODO: Write a good set of tests for tree depth. *)
let tree_depth_cps_tests : (int tree * int) list =
  [(Empty,0); ( Tree (Tree (Empty,1,Empty),1,Tree (Empty,1,Empty)) ,2); ( Tree (Empty,1,Tree (Tree (Empty,1,Empty),1,Empty)) ,3)]

(* An example of Non-CPS function to find depth of a tree: *)
let rec tree_depth t =
  match t with
  | Empty -> 0
  | Tree (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

(* TODO: Implement a CPS style tree_depth_cps function.*)
let tree_depth_cps (t: 'a tree) =
  let rec helper (t: 'a tree) (sc: (int -> int)) =
    match t with
    | Empty -> sc (-1)
    | Tree (l, _, r) -> helper l (fun ls -> helper r (fun rs -> sc (max (1+ls)  (1+rs)) )) 
  in
  helper t ((fun x -> x+1))

(* Question 2(a): Tree Traversal *)
(* TODO: Write a good set of tests for testing your tree traversal function. *) 
let traverse_tests : (int tree * int list) list = [(Empty,[]);(Tree (Tree(Empty,1,Empty),3,Tree(Empty,2,Empty)),[1;2;3]);(Tree (Tree(Empty,1,Empty),3,Empty),[1;3]);(Tree (Empty,3,Tree(Empty,2,Empty)),[2;3]);((Tree (Empty, 3, Tree (Tree (Empty, 6, Empty), 1, Tree (Tree (Empty, 7, Empty), 4, Tree (Empty, 2, Tree (Empty, 5, Empty)))))),[6; 7; 5; 2; 4; 1; 3])]

(* TODO: Implement a CPS style postorder traversal function. *) 
    
    (*THIS IS NOT CPS STYLE!!!! sc IS USELESS HERE*)


let traverse (tree : 'a tree) : 'a list =
  let rec helper (tree : 'a tree) (sc : 'a list -> 'r) =
    match tree with
    | Empty -> sc []
    | Tree (l, x, r) -> helper l (fun ls -> helper r (fun rs -> sc ((ls@rs)@[x])))
  in
  helper tree (fun x -> x)
    
    
(* Question 2(b): Distances from the Root *)
(* TODO: Write a good set of tests for testing the get_distances function. *)
let get_distances_tests : (int tree * int list) list = [(Empty,[]);(Tree (Tree(Empty,1,Empty),3,Tree(Empty,2,Empty)),[4;5;3]);(Tree (Tree(Empty,1,Empty),3,Empty),[4;3]);(Tree (Empty,3,Tree(Empty,2,Empty)),[5;3]);((Tree (Empty, 3, Tree (Tree (Empty, 6, Empty), 1, Tree (Tree (Empty, 7, Empty), 4, Tree (Empty, 2, Tree (Empty, 5, Empty)))))),[10; 15; 15; 10; 8; 4; 3])]

(* TODO: Implement a CPS style get_distances function. *)
let get_distances (tree : int tree) : int list = 
  let rec helper tree sum sc =
    match tree with
    | Empty -> sc []
    | Tree (l, x, r) -> helper l (x+sum) (fun ls -> helper r (x+sum) (fun rs -> sc ((ls@rs)@[x+sum])))
  in
  helper tree 0 (fun x -> x)

(* Question 3: Finding Subtrees *)
(* TODO: Write a good set of tests for finding subtrees. *)
let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [(([1],Empty),None);(([],Empty),Some Empty);(([],Tree (Empty,1,Empty)),Some (Tree (Empty,1,Empty)));(([2],Tree (Empty,1,Empty)),None);(([1],Tree (Empty,1,Empty)),Some Empty);(([1],Tree (Empty,1,Tree (Empty,2,Empty))),Some Empty);(([3;1;4],(Tree (Empty, 3, Tree (Tree (Empty, 6, Empty), 1, Tree (Tree (Empty, 7, Empty), 4, Tree (Empty, 2, Tree (Empty, 5, Empty))))))), Some (Tree(Empty,7,Empty)))]

(* TODO: Implement a CPS style find_subtree_cont function.*)
let find_subtree_cps ls tree =
  let rec helper ls tree sc fc = 
    match ls with
    | [] -> Some tree
    | x :: xs ->
        match tree with
        | Empty -> fc ()
        | Tree (l, m, r) -> if m!=x then fc () else
              helper xs l (fun l -> sc l) (fun () -> helper xs r sc fc)
  in
  helper ls tree (fun x -> x) (fun () -> None)
