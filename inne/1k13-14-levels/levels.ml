(* Dane są następujące definicje: *)

type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;;
let rec fold_tree f a t =
    match t with
    | Leaf -> a
    | Node (l, x, r) -> f x (fold_tree f a l) (fold_tree f a r);;

(* Napisz procedurę:

levels: 'a tree -> 'a list list

która dla danego drzewa zwraca listę list węzłów drzewa znajdujących się na
kolejnych poziomach głębokości. Listy składowe powinny być podane w
kolejności od korzenia wgłąb drzewa. Wartości z węzłów znajdujących się na
tej samej głębokości powinny być uporządkowane od lewej do prawej.

Rozwiązując to zadanie nie wolno Ci tworzyć żadnych procedur rekurencyjnych.
Zamiast tego należy wykorzystać procedurę fold_tree lub inne standardowe
procedury wyższych rzędów. *)

(*open List*)

let drzewo1 = Node(Leaf,1,Node(Leaf,2,Leaf));;
let drzewo2 = Node(Leaf,1,Node(Leaf,2,Leaf));;
let drzewo3 = Node(drzewo2,1,Node(Leaf,3,Node(Leaf,4,drzewo2)));;
let drzewo4 = Node(drzewo2,1,Node(Leaf,3,drzewo2));;
let drzewo5 = Node(drzewo2,1,Node(Leaf,3,Node(Leaf,1,drzewo2)));;

(*
let f v lv rv =
    match (lv, rv) with
    | ([],[]) -> [[v];[]]
    | (l,[]) | ([],l) -> [v]::l
    | (_,_) ->
        (* to nie zadziała, bo lewe poddrzewo może być znacznie głębsze od
        prawego. ewentualnym fixem byloby dobudowanie drzewa by dlugosc list
        sie zgadzala ale pod kątem kosztów pamięciowych byłoby to samobójstwo *)
        (*[v]::(List.map2 (fun a b -> a @ b) lv rv)*)
        [v]::[lv @ rv]
;;
*)
