(* [PCh] Dana jest definicja typu drzew binarnych: *)
type 'a tree = Node of 'a tree * 'a * 'a tree | Null
(* Powiemy, że węzeł drzewa jest środkowy, jeśli ma tyle samo węzłów przodków,
co węzłów potomków. Napisz procedurę srodkowe : 'a tree → 'a list, która dla
danego drzewa zwróci listę wartości znajdujących się w węzłach środkowych.
Wartości na liście wynikowej mogą być w dowolnej kolejności. *)

let srodkowe t =
    let rec aux tr depth lst = 
        match tr with
        | Null -> (lst, 0)
        | Node (l, x, r) ->
            let (lst1, size1) = aux l (depth + 1) lst in
            let (lst2, size2) = aux r (depth + 1) lst1 in
            
            if size1 + size2 = depth then
                (x::lst2, size1 + size2 + 1)
            else
                (lst2, size1 + size2 + 1)
    in
    fst (aux t 0 [])
;;

let drzewo1 = Node(Null,1,Node(Null,2,Null));;
let drzewo2 = Node(Null,1,Node(Null,2,Null));;
let drzewo3 = Node(drzewo2,1,Node(Null,3,Node(Null,4,drzewo2)));;
let drzewo4 = Node(drzewo2,1,Node(Null,3,drzewo2));;
let drzewo5 = Node(drzewo2,1,Node(Null,3,Node(Null,1,drzewo2)));;
