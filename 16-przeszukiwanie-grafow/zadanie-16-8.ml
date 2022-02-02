(* 8. Dany jest acykliczny graf skierowany (DAG). Jego wierzchołki są
ponumerowane od 0 do n, a krawędzie są dane w postaci tablicy sąsiedztwa
(kwadratowej tablicy e wartości logicznych; w grafie mamy krawędź u→v wtw., gdy
e.(u).(v) ).  Powiemy, że wierzchołek u dominuje wierzchołek v, jeżeli dla
każdego wierzchołka w, z którego można dojść do v, z u można dojść do w.

Napisz procedurę zdominowani : bool array array -> int, która na podstawie ta-
blicy opisującej graf obliczy liczbę wierzchołków, dla których istnieją
wierzchołki je dominujące. *)

(* utworzenie nowej kwadratowej tablicy domin.(u).(v) zliczającej dla danego u
   liczbę wierzchołków w prowadzących do v i później przejście po całej
   tablicy. Jeśli liczba wierzchołków w domin.(u).(v) zgadza się z liczbą sąsiadów
   danego v, to u dominuje v.
   Przewidywana złożoność T(n) = M(n) = Theta(n^2) *)

let zdominowani g =
    let n = Array.length g in
    let domin = Array.make_matrix n n 0
    and res = ref 0 in


