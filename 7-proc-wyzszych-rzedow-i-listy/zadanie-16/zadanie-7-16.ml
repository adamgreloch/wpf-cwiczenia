(* Dla danej listy liczb całkowitych l = [x1; x2; . . . ; xn], minimum
(maksimum) nazwiemy taki fragment listy x_i = x_(i+1) = ... = x_j, że:
• 1 ≤ i ≤ j ≤ n,
• jeśli i > 1, to x_i−1 > x_i (x_(i−1) < x_i),
• jeśli j < n, to x_j < x_(j+1) (x_j > x_(j+1)).
Ekstremum oznacza minimum lub maksimum. Napisz procedurę
ekstrema : int list → int
która dla danej listy policzy ile jest na niej ekstremów. *)

open List
let ekstrema l =
    let helper (res, pprev, prev) x = 
        if x = prev then
            (res, pprev, prev)
        else if (pprev < prev && prev > x) || (pprev > prev && prev < x) then
            (res+1, prev, x)
        else
            (res, prev, x)
    in
    match l with
    | [] -> 0
    | h::_ ->
    let (a,_,_) = fold_left helper (0, h, h) l in a
;;




