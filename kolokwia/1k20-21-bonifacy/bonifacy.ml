(* Notka z przyszłości: dało się to szybciej zrobić *)
open List

let bonifacy n lista = 
    let k = length lista in
    let rec aux i a b c =
        match n with
        | 0 -> 0
        | 1 | 2 -> 1
        | _ when n = i -> c
        | _ ->
            if nth lista (i mod k) = 0 then
                aux (i+1) b c (b+c)
            else
                aux (i+1) b c (a+c)
    in aux 3 0 1 1
;;
