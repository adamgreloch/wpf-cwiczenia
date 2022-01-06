(* 3. Na ile sposobów można wydać n zł reszty przyjmując, że w obrocie są dostępne monety
o zadanych (w postaci listy) nominałach. *)

open Hashtbl

let memoize tab f x =
    if mem !tab x then
        find !tab x
    else
        let wynik = f x
        in begin
            add !tab x wynik;
            wynik
        end

(* Kod zlicza wszystkie permutacje pewnego wyboru monet. Nie wiem, czy tak ma
   być. *)

let sposoby n nom =
    let a = Array.of_list nom
    and tab = ref (create 424242)
    in
    let rec f m = 
        if m < 0 then 0 else
        if m = 0 then 1 else
        let res = ref 0 in
        for i = 0 to Array.length a - 1 do
            res := !res + f (m - a.(i))
        done;
        !res
    in
    memoize tab f n
