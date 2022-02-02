(* 7. [PCh] Dany jest ciąg nawiasów, otwierających i zamykających. Napisz
   procedurę nawiasy, która obliczy minimalną liczbę nawiasów, które należy
   obrócić tak, aby uzyskać poprawne wyrażenie nawiasowe. Jeżeli nie jest to
   możliwe, to poprawnym wynikiem jest −1.

   val nawiasy : nawias list -> int *)

(* (())))()
   ()))((()
*)

type nawias = O | Z

let obroc = function O -> Z | Z -> O

let compare a b = match (a,b) with
    | (O,O) | (Z,Z) -> 0
    | (O,Z) -> -1
    | (Z,O) -> 1

(* T(n) = Theta(n) M(n) = Theta(n) *)
let nawiasy lst =
    let nawiasy = Array.of_list lst in
    let n = Array.length nawiasy in
    if n mod 2 <> 0 then -1 else
    let obrot = ref 0 in
    let count arr =
        let c = ref 0 in
        for i = 0 to n - 1 do
            if nawiasy.(i) = O then
                c := !c + 1
            else
                c := !c - 1;
            if !c < 0 then begin
                nawiasy.(i) <- (obroc nawiasy.(i));
                obrot := !obrot + 1;
                c := !c + 2
            end
        done;
    in
    count nawiasy;
    for i = 0 to n/2 do
        let buf = nawiasy.(i) in
        nawiasy.(i) <- obroc nawiasy.(n-1-i);
        nawiasy.(n-1-i) <- obroc buf
    done;
    count nawiasy;
    !obrot
