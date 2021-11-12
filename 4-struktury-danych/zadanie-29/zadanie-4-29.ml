(* 4-29 Drzewo nazywamy ultralewicowym jeśli głębokości kolejnych pustych
poddrzew (Null), od lewej do prawej, tworzą ciąg nierosnący. Napisz procedurę
ultraleft : α tree → bool, która sprawdza czy dane drzewo jest ultralewicowe. *)

type 'a tree =
    Node of 'a tree * 'a * 'a tree |
    Null;;

let ultraleft tree =
    let rec aux = function
        | Null -> (0, 0, true)
        | Node(l,_,r) ->
            let (l_max, l_min, lb) = aux l in
            if not lb then
                (0, 0, false)
            else
                let (r_max, r_min, rb) = aux r in
                (l_max + 1, r_max + 1, lb && rb && l_min >= r_max)
    in
    let (_,_,b) = aux tree in b
;;
