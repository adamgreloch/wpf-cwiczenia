(* Dana jest lista liczb całkowitych l = [x_1; . . . ; x_n] uporządkowana
niemalejąco. Napisz procedurę:
    malo : int list -> int
która dla danej listy l oblicza:
    min (|x_i + x_j| : 1 <= i < j <= n)
Na przykład, malo [-42, -12, -8, -1, -1, 5, 15, 60] = 2.
Możesz założyć, że dana lista l zawiera przynajmniej dwa elementy. *)

let list1 = [-60; -10; 2; 5; 61];;

open List

let malo l =
    let rec aux m l1 l2 curr_i curr_j n =
        let min1 = abs (hd l1 + curr_j)
        and min2 = abs (curr_i + hd l2)
        in
        if min1 < m then
            aux min1 (tl l1) l2 (hd l1) curr_j (n-1)
        else
            if min2 < m then
                aux min2 l1 (tl l2) curr_i (hd l2) (n-1)
            else
                if n < 2 then
                    m
                else
                    aux m (tl l1) (tl l2) (hd l1) (hd l2) (n-2)
    in
    let r = rev l in
    aux (abs (hd l + hd r)) (tl l) (tl r) (hd l) (hd r) (length l - 2)
;;
