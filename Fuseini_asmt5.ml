(*
CMPU-145, Spring 2026
Zaahid Fuseini
Asmt. 5
*)

(*PROBLEM "4a: It uses a helper fxn called join_elt to add the first element of listz to form a list of tuples and calls the cart_prod recursively on listy and rz"*)

(* function that adds elt to every to every list in s *)

let rec join_elt (elt : int) (s : int list) : (int * int) list =
    match s with
    | [] -> []
    | f :: r -> (f,elt) :: join_elt elt r
;; 

let rec cart_prod (listy : int list) (listz : int list) : (int * int) list =
    match listz with
    | [] -> []
    | fz :: rz -> join_elt fz listy @ (cart_prod listy rz)
;;

assert (cart_prod [1;2;3] [10;20] = [(1,10); (2, 10); (3, 10); (1, 20); (2, 20); (3, 20)]);;
assert (cart_prod [] [10; 20] = []);;
assert (cart_prod [1; 2] [] = []);;
assert (cart_prod [] [] = []);;
assert (cart_prod [1] [10] = [(1, 10)]);;

     
(*PROBLEM "4b: The helper fxn returns the a tuple of the two accumulators if tups is empty else it does a recursive call on rest of tups and takes or adds the first letters to accA or accB depending on whether it is already in the accumulator. The main fxn cart_opp calls opp_helper on tups and two empty lists which are accA and B "*)
let rec contains (n : int) (l : int list) : bool =
    match l with
    | [] -> false
    | f :: r -> if f = n then true else contains n r
;;

let rec opp_helper (tups : (int * int) list) (accA : int list) (accB : int list) : int list * int list =
    match tups with
    | [] -> (accA,accB)
    | (x,y)::r -> opp_helper r 
                  (if contains x accA then accA else accA @ [x]) 
                  (if contains y accB then accB else accB @ [y])
;;

                  
let rec cart_opp (tups : (int * int) list) : int list * int list =
    opp_helper tups [] []
;;
  
  
assert (cart_opp [(1,10); (2, 10); (3, 10); (1, 20); (2, 20); (3, 20)] = ([1;2;3],[10;20]));;  
assert (cart_opp [] = ([], []));;
assert (cart_opp [(5, 50)] = ([5], [50]));;
assert (cart_opp [(1, 10); (1, 20)] = ([1], [10; 20]));;
  
  
  
