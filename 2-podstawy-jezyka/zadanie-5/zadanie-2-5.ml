(* Liczbę naturalną nazwiemy rzadką, jeżeli w jej zapisie binarnym żadne dwie
jedynki nie stoją obok siebie. Napisz procedurę rzadkie : int → int, która dla
danej liczby naturalnej n zwróci najmniejszą rzadką liczbę naturalną większą od n. 
Na przykład, dla n = 42 = 101010_2 mamy rzadkie 42 = 1000000_2 = 64. *)

let rec rzadkie x =
    let rec jest x =
        if x = 0 then true
        else if x mod 4 = 3 then false
        (* Jeśli x daje resztę 3 przy dzieleniu przez 4 to znaczy, że w zapisie
           binarnym dwie ostatnie cyfry to jedynki (....11_2). Wówczas ta
           liczba nie może być rzadka. W przeciwnym wypadku przesuwamy się o
           jedną liczę w lewo i powtarzamy test podzielności. *)
        else jest (x/2)
    in
    if jest x then
        x
    else
        rzadkie (x+1)
;;

