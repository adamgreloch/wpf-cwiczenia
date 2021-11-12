let krazki rur kra =
    (* rur - rurkolista, kra - lista krążków *) 
    let rec spada k lr d = 
        match lr with
        | [] -> ([], d)
        | h::next::t ->
            if k <= next then
                let (l, dr) = spada k (next::t) (d+1) in
                (h::l, dr)
            else
                ([0], d+1)
        | h::[] -> ([0], d)
    in
    let rec wrzuc lr lk d =
        match lk with
        | [] -> d
        | h::t ->
            match (spada h lr 0) with
            | (hr::_, dr) when hr = 0 -> 
                if t = [] then dr else 0
            | (l, dr) -> wrzuc l t dr
    in
    wrzuc rur kra 0
;;

let rurka1 = [4;3;2;3;2;1]
let krazki1 = [2;1]

let rurka2 = [2;2;2;2;2]
let krazki2 = [1;1;1;1;1;1]
