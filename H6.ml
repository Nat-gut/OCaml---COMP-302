(*--------------------------------------------------------------*)
(* Q1 : String to Characters to String                  *)
(*--------------------------------------------------------------*)

(* 1.1 Turn a string into a list of characters. *)
let string_explode (s : string) : char list =
  tabulate (String.get s) (String.length s) 

(* 1.2 Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  String.concat "" (List.map Char.escaped l)

(* ------------------------------------------------------------------------*)
(* Q2 : Bank Account *)
(* ------------------------------------------------------------------------*)

let open_account (pass: password) : bank_account =
  let curr_balance = ref 0 in
  let curr_pass = ref pass in
  let wcount = ref 0 in
  let update_pass ops nps=
    if !wcount = 3 then
      raise account_locked
    else if ops = !curr_pass then
      (wcount := 0 ; curr_pass := nps)
    else
      (wcount := !wcount + 1 ; raise wrong_pass)
  in 
  let deposit ps toadd =
    if !wcount = 3 then
      raise account_locked
    else if ps = !curr_pass then
      if toadd >= 0 then
        (wcount := 0 ; curr_balance := !curr_balance + toadd)
      else
        raise negative_amount
    else
      (wcount := !wcount + 1 ; raise wrong_pass)
  in
  let retrieve ps toremove =
    if !wcount = 3 then
      raise account_locked
    else
    if ps = !curr_pass then
      if toremove >= 0 then
        if toremove <= !curr_balance then
          (wcount := 0 ; curr_balance := !curr_balance - toremove)
        else raise not_enough_balance
      else
        raise negative_amount
    else
      (wcount := !wcount + 1 ; raise wrong_pass)
  in
  let show_balance ps =
    if !wcount = 3 then
      raise account_locked
    else
    if ps = !curr_pass then
      (wcount := 0 ; !curr_balance)
    else
      (wcount := !wcount + 1 ; raise wrong_pass)
  in
  {
    update_pass = update_pass;
    retrieve = retrieve;
    deposit = deposit;
    show_balance = show_balance
  }
;;

