(* Napisz procedurę codrugi : α list → α list, która z danej listy wybiera co drugi
element (zaczynając od drugiego). Na przykład codrugi [1; 2; 3; 4; 5] = [2; 4]. *)

open List

let codrugi_long l =
    let helper (p, res) x =
        let res = if p then x::res else res in
            (not p, res)
    in
    let (_,a) = fold_left helper (false, []) l in rev a
;;

let codrugi1 l =
    let (a,_) = fold_left (fun (lp,p) x -> if p then (x::lp, not p) else (lp, not p))
    ([],false) l in rev a
;;

let codrugi2 l =
    let (a,_) = fold_right (fun x (lp,p) -> let lp = if p then x::lp else lp in (lp, not p))
    l ([],length l mod 2 = 0) in a
;;
