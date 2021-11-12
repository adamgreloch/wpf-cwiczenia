let zera_silni n =
    let rec aux x mod2 mod5 =
        match x with
        | 0 -> min mod2 mod5
        | _ ->
            if x mod 10 = 0 then
                aux (x-1) (mod2+1) (mod5+1) 
            else
                if x mod 5 = 0 then
                    aux (x-1) mod2 (mod5+1)
                else
                    if x mod 2 = 0 then
                        aux (x-1) (mod2+1) mod5
                    else
                        aux (x-1) mod2 mod5
    in
    aux n 0 0
;;

