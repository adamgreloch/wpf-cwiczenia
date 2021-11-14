(* Napisz procedurę wzrost : int list → int list, która dla danej listy liczb
całkowitych znajduje w niej najdłuższy spójny fragment ściśle rosnący i zwraca
go. Na przykład:
wzrost [3; 4; 0; -1; 2; 3; 7; 6; 7; 8] = [-1; 2; 3; 7]
wzrost [] = []
Rozwiązując to zadanie nie wolno Ci tworzyć żadnych własnych procedur
rekurencyjnych ani pętli. Możesz natomiast wykorzystać standardowe procedury
przetwarzające listy poznane na wykładzie. *)

open List

let wzrost l =
    let helper (res, curr, curr_len, max_len, prev) x =
        if x > prev || curr = [] then
                (res, x::curr, curr_len+1, max_len, x)
        else
            if curr_len > max_len then
                (curr, [], 0, curr_len, x)
            else
                (res, [], 0, max_len, x)
    in
    match l with
    | [] -> []
    | h::t -> let (a,_,_,_,_) = fold_left helper ([], [h], 1, 0, h) t in rev a
;;
