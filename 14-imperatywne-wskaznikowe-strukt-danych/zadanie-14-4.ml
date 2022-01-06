(* Dane są deklaracje typów: *)

type elem = { x : int; mutable prev : elem list }
type lista = elem list

(* Napisz procedurę ustaw : lista -> unit, która dla danej listy [x_1;x_2;...;x_n]
typu lista, tak ustawia pola prev, aby (dla i = 1,2,...,n) prowadziło ono z
elementu x_i do listy zaczynającej się w elemencie x_(n-i+1).

Rozwiązując to zadanie nie wolno Ci tworzyć żadnych własnych procedur
rekurencyjnych. Wolno natomiast korzystać z pętli i innych konstrukcji
imperatywnych. *)

open List

let create e = [{ x = e; prev = [] }];;

let add e l = l @ [{ x = e; prev = l }];;

let ustaw l =
    let prevs lista =
        let res = ref [] and tail = ref l in
        while !tail <> [] do
            begin
                res := (ref !tail)::(!res);
                tail := List.tl (!tail);
            end
        done;
        !res
    in
    List.iter2
    (fun elt p -> elt.prev <- !p)
    l (prevs l)
;;

