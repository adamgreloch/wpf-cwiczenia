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

open List

type 'a bush = Bush of 'a * 'a bush list;;

let rec fold_bush f (Bush(x,l)) =
    f x (map (fold_bush f) l)
;;

let fn a tab =
  function
    | []-> [a]::(List.fold_left (fun a f -> f a) [] tab)
    | h::t -> (a::h)::(List.fold_left (fun a f -> f a) t tab)
;;

let levels b =
    let create a fl fr = function
        | []-> [a]::(fl (fr []))
        | h::t -> (a::h)::(fl (fr t))
    in fold_tree create (fun x -> x) b []
;;


let zbierz b =
    let fn a tab = function
        | []-> [a]::(List.fold_right (fun f x -> f x) tab [])
        | h::t -> (a::h)::(List.fold_right (fun f x -> f x) tab t)
    in fold_bush fn b []
;;


let a = Bush(1, [Bush(2, []); Bush(3, [])])
let b = Bush(1, [Bush(2, [Bush(99, [Bush(98, [])])]); Bush(3, [Bush(4,[]);Bush(5,[]);Bush(6,[])])]);;

