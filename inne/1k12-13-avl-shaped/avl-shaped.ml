(* Dane są następujące definicje: *)

type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;;
let rec fold_tree f a t =
    match t with
    | Leaf -> a
    | Node (l, x, r) -> f x (fold_tree f a l) (fold_tree f a r);;

(* Mówimy, że drzewo ma kształt drzewa AVL, jeżeli dla każdego węzła drzewa
   wysokości jego obu poddrzew różnią się o co najwyżej 1. Napisz procedurę:
       avl_shaped: 'a tree -> bool
   która sprawdza, czy dane drzewo ma kształt drzewa AVL.
   Rozwiązując to zadanie nie wolno Ci tworzyć żadnych procedur rekurencyjnych.
   Zamiast tego należy wykorzystać procedurę fold_tree lub inne standardowe
   procedury wyższych rzędów. *)

let rec 
