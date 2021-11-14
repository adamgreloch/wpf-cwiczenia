(* http://smurf.mimuw.edu.pl/?q=wstep_do_programowania_funkcyjny_cw_9_10 *)

open List

exception Found

let lista = [1;2;3;4;5;6;7;8;9;10];;


let exists f l =
    try
        (fold_left (fun _ h -> if f h then raise Found) () l;
        false)
    with
    _ -> true
;;

let non = (fun f x -> not (f x));;

let forall f = 
    non (exists (non f));;

let append l1 l2 =
    fold_left (fun t h -> h::t) l2 (rev l1);;

let zlozenie lista = 
    fold_left (fun t h x -> h (t x)) (function x -> x) lista;;

let heads 
