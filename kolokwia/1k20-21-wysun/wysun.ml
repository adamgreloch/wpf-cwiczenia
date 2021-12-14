let a = [1;2;2;3;4;5;5;6;6;6;7];;

let wysun lista =
    match lista with
    | [] -> []
    | h::t ->
    let rec aux l multi prev single =
        match l with
        | [] -> List.rev (single @ multi)
        | h::[] -> aux [] multi (h-1) (h::single)
        | h1::h2::t -> if h1 = h2 then
            aux t (h1::h2::multi) h1 single
        else if h1 = prev then
            aux (h2::t) (h1::multi) h1 single
        else
            aux (h2::t) multi h1 (h1::single)
    in
    aux (h::t) [] (h-1) []
;;

