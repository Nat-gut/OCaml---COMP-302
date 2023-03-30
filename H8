(**To DO: Write a good set of tests for free_variables **)
let free_variables_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Let, Rec, Fn, and Apply!
  *)
  (Let ("x", I 3, (Let ("y", Var "x", Var "y"))), []); 
  (Fn ([("x",Int)],Var "x"),[]);
  (Rec ("x",Int,Var "x"),[]); 
  (Rec ("f",Int,Fn ([("x",Int)],Var "f")),[]);
  (Let ("x", I 4,Let ("f",Fn ([("y",Int)], Primop (Plus,[Var "x"; Var "y"])),Apply (Var "f",[I 3]))),[]) (*BROOOOOOOOOOOOOO CMOOOOON*);
  (Fn ([("y",Int)], Primop (Plus,[Var "x"; Var "y"])),["x"]);
  (Let ("x",Var "a",Primop (Plus,[Var"a";Var"x"])),["a"]);
  (Fn ([("x",Int);("y",Int);("z",Int)],Var "x"),[]);
  (Apply (Fn ([("x",Int);("y",Int);("z",Int)],Var "x"),[I 3;I 5;I 4]),[]);
]

(* TODO: Implement the missing cases of free_variables. *)
let rec free_variables : exp -> name list = 
  (* Taking unions of lists.
If the lists are in fact sets (all elements are unique),
then the result will also be a set.
  *)
  let union l1 l2 = delete l2 l1 @ l2 in
  let union_fvs es =
    List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es
  in
  function
  | Var y -> [y]
  | I _ | B _ -> []
  | If(e, e1, e2) -> union_fvs [e; e1; e2]
  | Primop (_, args) -> union_fvs args
  | Fn (xs, e) ->
      delete (List.map (fun (x,_)-> x) xs) (union_fvs [e])
  | Rec (x, _, e) ->
      delete [x] (union_fvs [e])
  | Let (x, e1, e2) ->
      delete ([x]) (union_fvs (union [e1] (delete [Var x] [e2])))
  | Apply (e, es) -> union_fvs [e] 
                                   (* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((Var "y","x"),Fn ([("x",Int);("y",Int)],Primop (Plus, [Var "x"; Var "y"; Var "z"]))),Fn ([("y",Int);("a",Int)],Primop (Plus, [Var "y"; Var "a"; Var "z"])));
  (((Var "y","x"),Rec ("x",Int, Primop (Plus, [Var "x"; Var "y"; Var "z"]))),Rec ("a",Int, Primop (Plus, [Var "a"; Var "y"; Var "z"])));
  (((I 5,"z"),Apply (Fn ([("x",Int);("y",Int)],Primop (Plus, [Var "x"; Var "y"; Var "z"])),[Var "y";Var "z"])),Apply (Fn ([("x",Int);("y",Int)],Primop (Plus, [Var "x"; Var "y"; I 5])),[Var "y"; I 5]));
  (((I 5,"z"),Fn ([("x",Int);("y",Int)],Primop (Plus, [Var "x"; Var "y"; Var "z"]))),Fn ([("x",Int);("y",Int)],Primop (Plus, [Var "x"; Var "y"; I 5])));
  (((I 5,"z"),Rec ("x",Int, Primop (Plus, [Var "x"; Var "y"; Var "z"]))),Rec ("x",Int, Primop (Plus, [Var "x"; Var "y"; I 5])));
  (((Primop (Plus,[Var "f";I 1]),"g"),Rec ("f",Int,Primop (Plus,[Var "f";Var "g"]))),Rec ("h",Int,Primop (Plus,[Var "h";Primop (Plus,[Var "f";I 1])])));
  (((Primop (Plus,[Var "f";I 1]),"g"),Fn ([("f",Int)],Primop (Plus,[Var "f";Var "g"]))),Fn ([("h",Int)],Primop (Plus,[Var "h";Primop (Plus,[Var "f";I 1])]))); 
]


(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      let (y, e) =
        if List.mem y (free_variables e) then
          rename y e
        else
          (y, e)
      in
      Rec (y, t, subst s e)

  | Fn (xs, e) ->
      if 
        List.mem x (List.map fst xs) then Fn (xs, e)
      else
        let (v,a) = rename_all (List.map (fun (x,_) -> x) xs) e in
        Fn (List.combine v (List.map (fun (_,x) -> x) xs), subst s a)
        
      
  | Apply (e, es) -> Apply (subst s e, (List.map (fun x -> subst s x) es))

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs
