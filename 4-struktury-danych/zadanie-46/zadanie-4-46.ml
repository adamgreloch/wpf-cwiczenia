(* Dana jest definicja typu drzew dowolnego stopnia: *)
type 'a tree = Node of 'a * 'a tree list;;
(* Napisz procedurę liscie : 'a tree → int list, która dla danego drzewa liczy
ile w tym drzewie jest liści na poszczególnych głębokościach i zwraca listę
liczb postaci [g_0; g_1; . . . ; g_h], gdzie gi to liczba liści znajdujących się
na głębokości i, a h to wysokość drzewa. *)
open List

let rec fold_tree f (Node(x,l)) =
    f x (map (fold_tree f) l)
;;

let liscie tree = 
    let aux v li acc =
        match acc with
        | [] -> (
            match li with
            | [] -> [1]
            | _ -> 0::List.fold_left (fun ak el -> el ak) [] li)
        | h::t -> (
            match li with
            | [] -> (h+1)::t
            | _ -> h::(List.fold_left (fun ak el -> el ak) t li))
    in
    fold_tree aux tree []
;;
