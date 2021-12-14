(* 
Dana jest tablica liczb całkowitych zawierająca permutację liczb całkowitych
od 0 do n (dla n >= 0). Napisz procedurę cykl : int array -> int, która
wyznacza długość najdłuższego cyklu danej permutacji. Twoja procedura nie
może zmieniać danej tablicy. Na przykład:

cykl [|2; 1; 0; 5; 6; 4; 3; 8; 7|] = 4
*)

let cykl arr =
    let n = Array.length arr in
    let visited = Array.make n false in
    let rec walk i j acc =
        (* acc = liczba krawędzi od i do j = liczba
        elementów na cyklu od i do j, ale bez j *)
        visited.(j) <- true;
        if i = j then acc
        else walk i arr.(j) (acc+1)
    in
    let i = ref 0 in
    let answ = ref 0 in
    while !i < n do (* n - wielkosc tablicy *)
        if not visited.(!i) then begin
            answ := max !answ (walk !i arr.(!i) 1)
        end;
        i := !i + 1;
    done;
    !answ
;;

cykl [|2; 1; 0; 5; 6; 4; 3; 8; 7|]
