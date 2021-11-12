open List

let a = [1;2;1;1]
(* a(x) = 1 + 2x + x^2 + x^3 *)
let b = [2;2]
(* b(x) = 2 + 2x *)
let c = [1;1]
(*
    (a*b)(x) = 2 + 4x + 2x^2 + 2x^3 + 2x + 4x^2 + 2x^3 + 2x^4
             = 2 + 6x + 6x^2 + 4x^3 + 2x^4
    res = [2;6;6;4;2]

    (a*c)(x) = x^4 + x^3 + 2x^2 + x + x^3 + x^2 + 2x + 1
             = x^4 + 2x^3 + 3x^2 + 3x + 1
    res = [1;3;3;2;1]
*)

let rec mnoz wm1 wm2 =
    if (length wm1 > length wm2) then
        match (wm1,wm2) with
        | (_,[]) | ([],_) -> []
        | (_,_) ->
            let rec dodaj w q =
                match (w,q) with
                | (_,[]) -> w
                | (h1::t1,h2::t2) -> (h1+h2)::(dodaj t1 t2)
                | (_,_) -> []
            in
            let rec przesun w n =
                match n with
                | 0 -> w
                | _ -> przesun (0::w) (n-1)
            in
            let rec przemnoz w q deg =
                match q with
                | [] -> w
                | h::t when h = 0 -> przemnoz w t (deg+1)
                | h::t ->
                    let res = przemnoz (map (fun a -> h*a) wm1) t (deg+1) in
                    if deg = 0 then
                        res
                    else
                        dodaj (przesun res deg) w
            in
            przemnoz wm1 wm2 0
    else
        mnoz wm2 wm1
;;
