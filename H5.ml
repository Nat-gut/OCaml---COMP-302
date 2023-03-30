(** Question 1 *)

(* TODO: Add test cases. *)
let q1a : Variable_set.t = 
  Variable_set.singleton "a"

let q1b : Variable_set.t
  = Variable_set.singleton "a"
    |> Variable_set.add "b"
    |> Variable_set.add "c"
    
let collect_variables_tests : (formula * Variable_set.t) list = [((parse_formula "a"), q1a);((parse_formula "(a & b) | ~c"), q1b)]

(* TODO: Implement the function. *) 

let rec collect_variables (formula : formula) : Variable_set.t =
  match formula with 
  | Conjunction (f1,f2) -> Variable_set.union (collect_variables f1) (collect_variables f2)
  | Disjunction (f1,f2) -> Variable_set.union (collect_variables f1) (collect_variables f2)
  | Negation f1 -> collect_variables f1 
  | Variable s -> Variable_set.singleton s
      
(** Question 2 *)

(* TODO: Add test cases. *)
let q2a : truth_assignment
  =  Variable_map.singleton "a" false
     |> Variable_map.add "b" true 
  
let eval_success_tests : ((truth_assignment * formula) * bool) list = [((q2a, parse_formula "a | b"),true);((q2a, parse_formula "b & b"),true)]

(* TODO: Add test cases. *)
let q2b : truth_assignment
  =  Variable_map.singleton "a" false
     |> Variable_map.add "b" true

let eval_failure_tests : ((truth_assignment * formula) * exn) list = [((q2b,parse_formula "a & b & x"),(Unassigned_variable "x"))]

(* TODO: Implement the function. *) 
       
let rec eval (state : truth_assignment) (formula : formula) : bool =
  match formula with
  | Conjunction (f1,f2) -> let u1 = eval state f1 in let u2 = eval state f2 in u1 && u2
  | Disjunction (f1,f2) -> let u1 = eval state f1 in let u2 = eval state f2 in u1 || u2
  | Negation f1 -> not (eval state f1)
  | Variable s -> match Variable_map.find_opt s state with
    | Some true -> true
    | Some false -> false
    | None -> raise (Unassigned_variable s)

(** Question 3 *)

(* TODO: Add test cases. *)
let q3a : truth_assignment
  =  Variable_map.singleton "a" true
     |> Variable_map.add  "b" false
       
let q3b : truth_assignment
  =  Variable_map.singleton "a" false
     |> Variable_map.add  "b" true
     

let find_satisfying_assignment_tests : (formula * truth_assignment option) list = [(parse_formula "a & ~a", None);(parse_formula "(a | b) & ~a", Some q3b)]

(* TODO: Implement the function. *)

let find_satisfying_assignment (formula : formula) : truth_assignment = 
  let var = collect_variables formula in 
  let varz = Variable_set.elements var in 
  let map = Variable_map.empty in
  let rec helper formul ls acc = 
    match ls with
    | [] ->
        if eval acc formul = true then acc else raise Unsatisfiable_formula
    | c :: cs -> 
        try 
          helper formul cs (Variable_map.add c true acc) 
        with
        | Unsatisfiable_formula -> 
            helper formul cs (Variable_map.add c false acc)
  in
  helper formula varz map