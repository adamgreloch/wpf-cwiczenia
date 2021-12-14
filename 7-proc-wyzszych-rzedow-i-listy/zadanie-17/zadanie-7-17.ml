(* Napisz procedurę prefiksy : int list → int list list, która dla danej listy
liczb całkowitych zwróci listę wszystkich jej prefiksów zakończonych zerem, w
kolejności rosnącej ich długości. Na przykład:
prefiksy [0;1;2;3;0;5;6;0;1] = [[0]; [0;1;2;3;0]; [0;1;2;3;0;5;6;0]] *)

open List

let prefiksy l =
    let f (res, curr) h =
        let res = if h = 0 then
            rev (h::curr)::res else res
        in
            (res, h::curr)
    in
    rev (fst (fold_left f ([], []) l))
;;

