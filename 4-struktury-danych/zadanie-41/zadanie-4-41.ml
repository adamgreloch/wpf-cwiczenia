(* Dana jest deklaracja typu drzew binarnych: *)

type tree = Node of tree * int * tree | Null;;

(* Napisz procedurę czapeczka : tree → tree → int, która obliczy na ilu
poziomach, idąc od góry, dane drzewa są identyczne, tzn. na tych poziomach
drzewa mają ten sam kształt, a w odpowiadających sobie wierzchołkach są takie
same liczby. *)

let drzewo1 = Node(Null,1,Node(Null,2,Null));;
let drzewo2 = Node(Null,1,Node(Null,2,Null));;
let drzewo3 = Node(drzewo2,1,Node(Null,3,Node(Null,4,drzewo2)));;
let drzewo4 = Node(drzewo2,1,Node(Null,3,drzewo2));;
let drzewo5 = Node(drzewo2,1,Node(Null,3,Node(Null,1,drzewo2)));;

let czapeczka tree1 tree2 =
    let rec aux t1 t2 (res, full) =
        match (t1, t2) with
        | (Null,Null) -> (res, true)
        | (Node(l1,x1,r1), Node(l2,x2,r2)) ->
                if x1 = x2 then
                    let (r_res, r_full) = aux r1 r2 (res+1, false)
                    and (l_res, l_full) = aux l1 l2 (res+1, false)
                    in
                    if (l_full && r_full) then
                        (max l_res r_res, true)
                    else
                        if l_full then
                            (r_res, false)
                        else
                            (l_res, false)
                else
                    (res, false)
        | (_,_) -> (res, false)
    in
    fst (aux tree1 tree2 (0, false))
;;
    
