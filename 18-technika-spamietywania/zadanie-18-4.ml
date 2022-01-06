(* 4. Dana jest kwota k oraz zestaw nominałów banknotów b = [b1; b2; ...; bn].
Mamy dowolną liczbę banknotów każdego z nominałów. Napisz procedurę banknoty :
int -> int list -> int, która dla danych k i b obliczy minimalną liczbę
różnych nominałów banknotów, jakich należy użyć do wydania kwoty k. Jeżeli
nie da się wydać kwoty k, używając podanych nominałów, to procedura powinna
zwrócić −1.

Na przykład, banknoty 49 [10, 15, 11, 12, 20] = 3. Liczbę 49 można uzyskać na
dwa sposoby: 49 = 12 + 12 + 10 + 15 = 11 + 11 + 15 + 12. *)

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

let banknoty n nom =
    let a = Array.of_list nom
    and tab = ref (create 424242)
    in
    let rec f m occ = 
        if m < 0 then max_int/2 else
        if m = 0 then 0 else
        let res = ref max_int in
        for i = 0 to Array.length a - 1 do
            if List.mem a.(i) occ then
                res := min !res (f (m - a.(i)) occ)
            else
                res := min !res (1 + f (m - a.(i)) (a.(i)::occ))
        done;
        !res
    in
    match memoize tab f n [] with
    | answ when answ > max_int/2 -> -1
    | answ -> answ

