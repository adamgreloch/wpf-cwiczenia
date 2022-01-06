(* 6. Napisz procedurę silnie : int -> int list, która dla danej dodatniej
liczby całkowitej n znajduje najkrótszy taki ciąg dodatnich liczb całkowitych
[x_1; x_2; ...; x_k], że x_1! + x_2! + ... + x_k! = n.
Na przykład, dla silnie 42 poprawnym wynikiem jest n.p. [3; 4; 3; 3]. *)

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

let fact =
    let tab = ref (create 424242)
    in let rec f n =
        memoize tab (function n -> if n < 2 then 1 else n * (f (n-1))) n
    in f

let silnie n =
    let rem = ref n
    and res = ref []
    and k = ref 1
    in
    while !rem > 0 do
        if !rem = 1 then begin
            res := 1::!res;
            rem := 0
        end
        else if fact !k <= !rem then (* zamiast tego mozna zrobic szybka silnie
        jako sume czesciowa *)
            k := !k + 1
        else begin
            res := (!k-1)::!res;
            rem := !rem - fact (!k-1);
            k := 1
        end
    done;
    !res


