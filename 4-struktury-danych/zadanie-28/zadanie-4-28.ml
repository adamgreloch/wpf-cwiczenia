(* 4-28: Zdefiniuj typ reprezentujący drzewa o wierzchołkach dowolnego
(skończonego) stopnia.  Zdefiniuj garść procedur operujących na takich drzewach
(np.  głębokość, liczbę elementów, lista elementów w porządku
prefiksowym/postfiksowym). *)

type 'a tree =
    Node of 'a tree * 'a * 'a tree |
    Null;;

let depth tree =
    let rec aux t res =
        match t with
        | Null -> res
        | Node(l, _, r) -> max (aux l (res+1)) (aux r (res+1))
    in
    aux tree 0
;;

let count tree =
    let rec aux t =
        match t with
        | Null -> 0
        | Node(l, _, r) -> (aux l) + (aux r) + 1
    in
    aux tree
;;

(*
https://open.spotify.com/album/0gGlfEhMI6yS0wQU4hvxWr?si=Cr5TwcDOScahVcHsIx5kcQ
*)

let syny_prefix tree =
    let rec aux t res = 
        match t with
        | Null -> res
        | Node(l, v, r) -> aux r (aux l (res @ [v]))
    in
    aux tree []
;;

let syny_infix tree =
    let rec aux t res =
        match t with
        | Null -> res
        | Node(l, v, r) -> aux r ((aux l res) @ [v])
    in aux tree []
;;

let syny_postfix tree =
    let rec aux t res =
        match t with
        | Null -> res
        | Node(l, v, r) -> (aux r (aux l res)) @ [v]
    in aux tree []
;;

let a = Node( Node( Node( Null, 3, Node( Null, 4, Null)), 2, Null), 1, Node(
    Null, 2, Null));;

let b = Node( Node( Node( Node(Null,(3,0),Null), (2,0), Node( Null, (2,1),
Null)), (1,0), Null), (0,0), Node( Null, (0,1), Null));;

