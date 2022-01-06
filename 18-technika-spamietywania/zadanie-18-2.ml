(* 2. Jaka jest minimalna liczba monet lub banknotów potrzebna do wydania n zł
reszty, przyjmując, że monety mogą być przekazywane w obie strony. (Na
przykład, żeby otrzymać 9 zł mogę dać 1 zł i otrzymać 10 zł, razem 2). W
obrocie są dostępne monety i banknoty o zadanych (w postaci tablicy) nominałach. *)

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

let reszta n a =
    let tab = ref (create 424242)
    in
    let rec f m = 
        if m < 0 then max_int/2 else
        if m = 0 then 0 else
        let res = ref max_int in
        for i = 0 to Array.length a - 1 do
            res := min !res (1 + f (m - a.(i)));
            for j = 0 to Array.length a - 1 do
                if a.(i) - a.(j) > 0 then
                res := min !res (2 + f (m - a.(i) + a.(j)));
            done
        done;
        !res
    in
    match memoize tab f n with
    | answ when answ > max_int/2 -> -1
    | answ -> answ

