(* Listę nazwiemy ciekawą jeśli żadne dwa kolejne jej elementy nie są sobie
równe. Napisz procedurę podzial : α list → α list list, która daną listę
podzieli na minimalną liczbę spójnych fragmentów (zachowując ich kolejność), z
których każdy jest ciekawy.  Na przykład, podzial [3; 2; 2; 5; 7; 5; 4; 4; 3;
1] = [[3; 2]; [2; 5; 7; 5; 4]; [4; 3; 1]]. *)

let podzial l =
    let f x a =
        match a with
        | (h::_ as pierw)::t when h <> x -> (x::pierw)::t
        | _ -> [x]::a
    in
    List.fold_right f l []
;;
