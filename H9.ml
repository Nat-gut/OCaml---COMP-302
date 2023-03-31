(* Write some SUCCESS test cases for `infer` *)
let infer_tests : ((ctx * exp) * tp) list = [
  (([("x", Int);("y", Int)], (Primop (Plus,[Var "x";Var "y"]))), Int);
  (([("x", Int);("y", Int)],(Apply (Fn ([("x", Int);("y", Int)],Var "x"),[I 6; I 7]))),Int);
  (([("x", Int)],(Apply (Fn ([("x", Int)],Var "x"),[I 6]))),Int);
  (([],(Fn ([],I 5))), Arrow ([],Int));
  (([("x", Int);("y", Bool);("z", Bool)],Fn ([("x", Int);("y", Bool);("z", Bool)],If (Primop (Equals,[Var "x"; I 5]),Var "y",Var "z"))),(Arrow ([Int; Bool; Bool], Bool)));
  (([("x", Int)],Rec ("f",Int,Primop (Plus,[Var "f";Var "x"]))),Int);
  (([],Apply (Fn ([],I 5),[])), Int);
  (([("x", Int);("y",Int)],Fn ([("x", Int)],Primop (Plus,[Var "x";Var "y"]))),Arrow ([Int],Int))
]

(* Q1: `infer_op` - Type Inference in Primitive Operations *)

(* [infer_op] returns the type of a primitive operation *)
let infer_op (op : primop) (ts : tp list) : tp =
  begin match op with
    | Equals -> 
        if List.length ts <> 2 then raise ArityMismatch 
        else 
          begin match ts with
            | Bool :: x ->
                begin match x with
                  | Bool :: _ -> Bool
                  | _ -> raise TypeMismatch
                end
            | Int :: x ->
                begin match x with
                  | Int :: _ -> Bool
                  | _ -> raise TypeMismatch
                end
            | _ -> raise TypeMismatch
          end
    | LessThan ->
        if List.length ts <> 2 then raise ArityMismatch 
        else 
          begin match ts with
            | Int :: x ->
                begin match x with
                  | Int :: _ -> Bool
                  | _ -> raise TypeMismatch
                end
            | _ -> raise TypeMismatch
          end
    | Plus ->
        if List.length ts <> 2 then raise ArityMismatch 
        else 
          begin match ts with
            | Int :: x ->
                begin match x with
                  | Int :: _ -> Int
                  | _ -> raise TypeMismatch
                end
            | _ -> raise TypeMismatch
          end
    | Minus ->
        if List.length ts <> 2 then raise ArityMismatch 
        else 
          begin match ts with
            | Int :: x ->
                begin match x with
                  | Int :: _ -> Int
                  | _ -> raise TypeMismatch
                end
            | _ -> raise TypeMismatch
          end
    | Times ->
        if List.length ts <> 2 then raise ArityMismatch 
        else 
          begin match ts with
            | Int :: x ->
                begin match x with
                  | Int :: _ -> Int
                  | _ -> raise TypeMismatch
                end
            | _ -> raise TypeMismatch
          end
    | Negate ->
        if List.length ts <> 1 then raise ArityMismatch 
        else 
          begin match ts with
            | Int :: _ -> Int
            | _ -> raise TypeMismatch
          end
  end

(* Q2: `infer` - General Type Inference *)

(* [infer] returns the type of an expression in a context *)
let rec infer (ctx : ctx) (e : exp) : tp =
  match e with
  | I _ -> Int
  | B _ -> Bool
  | Var x -> 
      begin match List.assoc_opt x ctx with
        | None -> raise FreeVariable
        | Some t -> t
      end
  | Primop (op, es) ->
      infer_op op (List.map (fun x -> infer ctx x) es)
  | If (cond, e1, e2) ->
      begin match infer ctx e1, infer ctx e2 with
        | t1, t2 when t1 = t2 -> t1
        | _ -> raise TypeMismatch
      end
  | Let (x, e1, e2) -> 
      let t1 = infer ctx e1 in
      infer ((x,t1)::ctx) e2
  | Fn (xs, e') ->
      let t2 = infer (xs @ ctx) e' in
      Arrow ((List.map (fun (_,t1)-> t1) xs), t2)
  | Apply (e', args) -> 
      begin match infer ctx e' with
        | Arrow (t, t') ->
            let xs = List.map
                (fun e2 -> infer ctx e2)
                args
            in
            if 
              List.length xs <> List.length t then raise ArityMismatch
            else if
              xs <> t then raise TypeMismatch 
            else
              t'
        | _ -> raise TypeMismatch
      end
  | Rec (f, t, e') -> 
      let t2 = infer ((f, t) :: ctx) e' in
      if 
        t2<>t then raise TypeMismatch 
      else
        t2
