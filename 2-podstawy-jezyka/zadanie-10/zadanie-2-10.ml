(* Napisz procedurę, która dla danej liczby n sprawdzi czy pierścień reszt
modulo n zawiera nietrywialne pierwiastki z 1
(tj. takie liczby k, k ̸= 1, k ̸= n−1, że k^2 ≡ 1 mod n). *)

let nietrywialne n =
    if n < 3 then
        false
    else
    let rec aux k i =
        if i = n - 1 then
            false
        else if k mod n = 1 && i > 1 then
            true
        else
            aux (k + 2*i + 1) (i+1)
    in
    aux 4 2
;;
