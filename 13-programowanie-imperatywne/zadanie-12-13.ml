(* [PCH] Dana jest tablica liczb całkowitych a = [|x_1;x_2;...;x_n|]. Tablica ta ma taką
własność, że liczby x_{i+1} − x_i , dla i = 1,2,...,n−1, tworzą ciąg rosnący.

- Napisz procedurę rozne : int array -> bool, która dla danej tablicy sprawdza,
czy jej elementy są parami różne.
- Napisz procedurę minimum : int array -> int, która dla danej tablicy wyznacz
minimum z jej elementów.

Rozwiązując to zadanie nie wolno Ci tworzyć żadnych własnych procedur rekurencyj-
nych. Wolno natomiast korzystać z pętli i innych konstrukcji imperatywnych.
Podaj złożoność czasową i pamięciową swojego rozwiązania.
*)

(* T(n) = O(n) = M(n) *)

let rozne a = 
    let i = ref 0
    and res = ref true
    and n = Array.length a in
    let pref = Array.make n 0 in
    begin
        while !i < n-1 do
            begin
                pref.(!i+1) <- pref.(!i) + a.(!i+1) - a.(!i);
                if pref.(!i+1) = 0 then res := false;
                i := !i + 1
            end
        done;
        !res
    end
;;
