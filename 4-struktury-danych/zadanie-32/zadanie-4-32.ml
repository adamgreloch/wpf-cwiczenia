(* 4-32: Napisz procedurę, która dla dowolnego drzewa binarnego poprawia je
tak, że spełnia ono słabszą wersję warunku BST: dla dowolnego węzła, lewy syn
nie jest większy, a prawy nie jest mniejszy, niż węzeł. *)

type 'a tree =
    Node of 'a tree * 'a * 'a tree |
    Null;;

let weak_bst tree =
    let rec insert x = function
        | Null -> Node(Null, x, Null)
        | Node(l, h, r) as t ->
            if x < h then Node(insert x l, h, r)
            else if h < x then Node(l, h, insert x r)
            else t
    in
    let rec popraw res = function
        | Null -> res
        | Node(l, h, r) ->
            popraw (popraw (insert h res) l) r
    in
        popraw Null tree
;;
