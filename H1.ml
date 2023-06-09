(* Question 1: Manhattan Distance *) 
let distance_tests = [ 
  (((8,3), (8,3)), 0); (((-1,-2), (-2,-3)), 2); (((1, 2), (3, 4)), 4); (((1, 10000), (1, 20000)), 10000); (((-3, -2), (-3, -2)), 0); (((-3, -3), (-3, -3)), 0);  (((3, 3), (3, 3)), 0); (((10000, 1), (20000, 1)), 10000); (((20000, 1), (10000, 1)), 10000); (((1, 20000), (1, 10000)), 10000)
]
;;

let distance (x1, y1) (x2, y2) = 
  (abs (x1 - x2)) + (abs (y1 - y2))
;;

(* Question 2: Binomial *) 
let binomial_tests = [
  ((1, 1), 1); ((12, 1), 12); ((7, 4), 35); ((8, 2), 28); ((8, 6), 28); ((9, 9), 1); ((6, 0), 1); ((0, 0), 1); ((8, 4), 70); ((1, 0), 1)
]
;;

let rec factorial n = 
  if n = 0 then 1 else n*factorial (n-1)
;;
    
let binomial n k = 
  (factorial n)/((factorial k)*(factorial (n-k)))
;;


(* Question 3: Lucas Numbers *) 
let lucas_tests = [ 
  (0, 2); (1, 1)
]
;;

let rec lucas_helper n k a b =
  if k = n then a else
    lucas_helper n (k+1) b (a+b)
;;

let lucas n =
  lucas_helper n 0 2 1
;;