(* Dla danej listy liczb całkowitych l = [x1; x2; . . . ; xn], minimum
(maksimum) nazwiemy taki fragment listy x_i = x_(i+1) = ... = x_j, że:
• 1 ≤ i ≤ j ≤ n,
• jeśli i > 1, to x_i−1 > x_i (x_(i−1) < x_i),
• jeśli j < n, to x_j < x_(j+1) (x_j > x_(j+1)).
Ekstremum oznacza minimum lub maksimum. Napisz procedurę
ekstrema : int list → int
która dla danej listy policzy ile jest na niej ekstremów. *)

let ekstrema l =
    let helper (res, 
