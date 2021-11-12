(* Dana jest deklaracja typu drzew binarnych: *)

type 'a tree = Node of 'a * 'a tree * 'a tree | Null;;

(* Skosokość drzewa jest zdefiniowana w następujący sposób:
    • skosokość Null wynosi 0,
    • skosokość Node(x, t1, t2) jest równa max(2·s1, 2·s2 +1), gdzie s1 i s2 to
    skosokości, odpowiednio, t1 i t2.
Dla danego drzewa wolno nam w dowolnych jego węzłach zamieniać miejscami lewe i
prawe poddrzewa. Napisz procedurę skosokosc : α tree → α tree, która
przekształca dane drzewo tak, aby zminimalizować jego skosokość. *)

let t = Node(5, Node(4, Null, Node(2, Null, Null)), Node(6, Node(1, Null,
Null), Node(3, Null, Null)))

let skosokosc tree = 
    let rec aux t = 
        match t with
        | Null -> (0, Null)
        | Node(x, l, r) ->
            let (s_l, new_l) = aux l and (s_r, new_r) = aux r in
            if s_l >= s_r then
                (max (2*s_l) (2*s_r + 1), Node(x, new_l, new_r))
            else
                (max (2*s_r) (2*s_l + 1), Node(x, new_r, new_l))
    in snd (aux tree)
;;

