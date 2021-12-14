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

let nie_avl = Node(Node(Node(Node(Leaf,4,Node(Leaf,5,Leaf)),3,Leaf),2,Leaf),1,Leaf);;
let avl1 = Node(Node(Leaf,1,Leaf),0,Leaf);;
let avl2 = Node(Node(Node(Leaf,4,Leaf),2,Node(Leaf,5,Leaf)),1,Node(Leaf,3,Leaf));;
let avl3 = Node(Node(Leaf,4,Leaf),2,Node(Leaf,5,Node(Leaf,6,Leaf)));;
let avl2_a = Node(avl3, 1, Node(Leaf,3,Node(Leaf,4,Node(Leaf,1,Leaf))));;

let avl_shaped tree =
    try
    ignore (fold_tree (fun _ lb rb ->
        if (abs(lb - rb) > 1) then
            raise Exit
        else
            max (lb+1) (rb+1)) 0 tree); true 
    with Exit -> false
;;

let height = fold_tree (fun _ l r -> max (l+1) (r+1)) 0;;

