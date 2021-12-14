type expr = Add of expr * expr | Var of string

let rec fold_expr f_add f_var = function
    | Add (l, r) -> f_add (fold_expr f_add f_var l) (fold_expr f_add f_var r)
    | Var z -> f_var z

let rec st = function
    | Add (l,r) -> max (st l) (1 + st r)
    | Var _ -> 1

(* T(n) = M(n) = O(n) gdzie n - liczba wyrażeń arytmetycznych *)

let optymalizuj =
    fold_expr (fun l r ->
        if st l < st r then
            Add (r, l)
        else
            Add (l, r))
    (fun x -> Var x)
;;

optymalizuj (Add (Var "a", Add (Var "b", Var "c")))
