(* Dana jest niepusta, rosnąca lista liczb całkowitych l = [x1; . . . ; xn].
Napisz procedurę:
    fit : int → int list → int
która dla danej liczby c i listy l obliczy:
    min (|c + xi + xj| : 1 ≤ i, j ≤ n)
Na przykład, fit 42 [-28, -25, -15, -1, 4, 8, 15, 60] = 1, ponieważ |42 − 28 −
15| = 1. *)

open List

let fit c l =
    let rec aux m nl nr i_curr j_curr n =
        let min1 = abs(c + hd nl + j_curr)
        and min2 = abs(c + i_curr + hd nr)
        and min3 = abs(c + hd nl + hd nr)
        in
        if min1 < m then
            aux min1 (tl nl) nr (hd nl) j_curr (n-1)
        else if min2 < m then
            aux min2 nl (tl nr) i_curr (hd nr) (n-1)
        else if n < 2 then
            m
        else
            aux min3 (tl nl) (tl nr) (hd nl) (hd nr) (n-2)
    in
    let r = rev l in
    aux (abs c + hd l + hd r) (tl l) (tl r) (hd l) (hd r) (length l - 2)
;;

