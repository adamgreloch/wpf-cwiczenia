(* [PCh] Dana jest deklaracja typu drzew binarnych: *)

type tree = Node of tree * int * tree | Null;; 

(* Napisz procedurę naprzemienne : tree → bool, która
sprawdza, czy wartości występujące na każdej ścieżce od korzenia do liści
mają naprzemienne znaki. Przyjmujemy przy tym, że zero jest równocześnie
dodatnie i ujemne, tzn. po zerze może następować dowolna liczba. Na przykład,
następujące liczby mają naprzemienne znaki: 2, -1, 3, 0, -2, 0, 0, 7, 0, 4. *)

let naprzemienne tree = 

