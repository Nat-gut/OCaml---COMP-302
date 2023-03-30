(* Question 1 *)

let q1a_nat_of_int_tests : (int * nat) list = [(0, Z); (1, (S Z))] 
                                              
let q1a_nat_of_int (n : int) : nat =
  let rec q1a_helper (n : int) (acc : nat) =
    if n = 0 then acc else
      q1a_helper (n-1) (S (acc))
  in
  q1a_helper n Z

let q1b_int_of_nat_tests : (nat * int) list = [(Z,0); ((S Z), 1)] 
  
let q1b_int_of_nat (n : nat) : int =
  let rec q1b_helper (n : nat) (acc : int) : int =
    match n with
    | Z -> acc
    | S n -> q1b_helper n (1+acc)
  in 
  q1b_helper n 0
(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [( (Z,Z) , Z );( (S (Z),Z) , S (Z) );( (Z,S (Z)) , S (Z) )]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat =
  match n with
  | Z -> m
  | S n' -> q1c_add n' (S (m))


(* Question 2 *)

let q2a_neg (e : exp) : exp =
  Times (
    Const (-1.0),
    e
  )

let q2b_minus (e1 : exp) (e2 : exp) : exp =
  Plus (
    e1, 
    q2a_neg e2
  )
    
let rec q2c_pow (e1 : exp) (p : nat) : exp =
  match p with
  | Z -> Const 1.0
  | S (Z) -> Times(
      e1,
      Const 1.0
    )
  | S (S (p')) -> Times(
      q2c_pow e1 (S(p')),
      e1
    )
  


(* Question 3 *)

let eval_tests : ((float * exp) * float) list = [((1.0,Var),1.0);((1.0,Const 2.0),2.0);((2.0,Times(Var,Var)),4.0);((4.0,Div(Var,Const 2.0)),2.0);((4.0,Plus(Var,Const 3.0)),7.0)]

let rec eval (a : float) (e : exp) : float =
  match e with
  | Var -> a
  | Const f -> f
  | Plus(e1,e2) -> (eval a e1) +. (eval a e2)
  | Times(e1,e2) -> (eval a e1) *. (eval a e2)
  | Div(e1,e2) -> (eval a e1) /. (eval a e2)


(* Question 4 *)

let diff_tests : (exp * exp) list = [(Var, Const 1.0);(Const 3.0, Const 0.0);(Plus(Times(Const 2.0, Var),Const 4.0), Plus(Plus(Times(Const 2.0, Const 1.0),Times(Const 0.0, Var)),Const 0.0));(Times(Var,Var), Plus(Times(Const 1.0, Var),Times(Var, Const 1.0)));(Div(Const 1.0,Var), Div(Plus(Times(Const 0.0, Var),Times(Const (-1.0),Times(Const 1.0, Const 1.0))),Times(Var,Var)))]

let rec diff (e : exp) : exp = 
  match e with 
  | Var -> Const 1.0
  | Const f -> Const 0.0
  | Plus(e1, e2) -> Plus(diff e1, diff e2)
  | Times(e1,e2) -> Plus(Times(diff e1, e2),Times(e1, diff e2))
  | Div(e1,e2) -> Div(Plus(Times(diff e1, e2),Times(Const (-1.0),Times(e1, diff e2))),Times(e2,e2))
