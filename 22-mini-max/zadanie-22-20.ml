(* 20. Rozważamy drzewa postaci: *)

type expr =
    | NWD of expr * expr
    | NWW of expr * expr
    | Number of int

(* Drzewa takie reprezentują wyrażenia. NWD oznacza największy wspólny
dzielnik, a NWW najmniejszą wspólną wielokrotność dwóch podwyrażeń.
Argumentami Number są liczby nieujemne. Number x, dla x > 0 oznacza liczbę x.
Number 0 oznacza miejsce, w które należy wstawić dodatnią liczbę.

Napisz procedurę wstaw : expr * int -> int, która dla danego drzewa t i
dodatniej liczby całkowitej n znajdzie taką najmniejszą dodatnią liczbę
całkowitą x, że gdy wstawimy Number x w miejsce wszystkich wystąpień Number 0 w
t, to wartością wyrażenia będzie n. Jeżeli taka liczba x nie istnieje, to
poprawnym wynikiem jest 0.  Na przykład, dla t = NWW(NWW(Number 2, Number 0),
NWD(Number 0, Number 49)) i n = 42, wstaw t n = 21. *)
