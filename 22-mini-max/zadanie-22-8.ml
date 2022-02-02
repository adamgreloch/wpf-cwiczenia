module type MAP =
    sig
    (* Mapa z wartości typu 'a w 'b. *)
    type ('a,'b) map

    (* Wyjątek podnoszony,gdy badamy wartość spoza dziedziny. *)
    exception Undefined

    (* Pusta mapa. *)
    val empty : ('a,'b) map

    (* Predykat charakterystyczny dziedziny mapy. *)
    val dom : ('a,'b) map -> 'a -> bool

    (* Zastosowanie mapy. *)
    val apply : ('a,'b) map -> 'a -> 'b

    (* Dodanie wartości do mapy. *)
    val update : ('a,'b) map -> 'a -> 'b -> ('a,'b) map
end;;

module Map : MAP =
    struct
    (* Mapa z wartości typu 'a w 'b. *)
    type ('a,'b) map =
        Empty |
        Node of ('a * 'b * ('a,'b) map * ('a,'b) map)

    (* Wyjątek podnoszony,gdy badamy wartość spoza dziedziny. *)
    exception Undefined

    (* Pusta mapa to puste drzewo *)
    let empty = Empty

    (* Znajdź poddrzewo o zadanym kluczu *)
    (* (jeśli nie ma,to wynikiem jest puste drzewo). *)
    let rec find tree key =
        match tree with
            Empty -> Empty |
            Node (k,_,l,r) ->
            if key = k then tree
            else if key < k then find l key
            else find r key

    (* Zastosowanie mapy. *)
    let apply m k =
        match find m k with
            Empty -> raise Undefined |
            Node (_,v,_,_) -> v

    (* Sprawdzenie,czy punkt należy do dziedziny *)
    let dom m x =
        try
            let _ = apply m x
            in true
        with
            Undefined -> false

    (* Zmiana wartości mapy w punkcie *)
    let rec update tree key value =
        match tree with
            Empty -> Node (key,value,Empty,Empty) |
            Node (k,v,l,r) ->
            if key = k then
                Node (key,value,l,r)
            else if key < k then
                Node (k,v,update l key value,r)
            else
                Node (k,v,l,update r key value)
end

let memoize tab f x =
    if Map.dom !tab x then
        Map.apply !tab x
    else
        let wynik = f x
        in begin
            tab := Map.update !tab x wynik;
            wynik
        end

let pow x = x*x

(* T(n) = M(n) = O(k*n^2) *)

let myszy k arr =
    let tab = ref Map.empty
    and n = Array.length arr in
    let prf = Array.make (n+1) 0 in
    let rec dp j l =
        if j = 0 || l = 0 then 0 else
        let res = ref (min_int) in
        for i = 1 to j do
            res := max (!res) ((dp (i-1) (l-1)) + prf.(j) - prf.(i) - pow (j-i-1))
        done;
        max (dp (j-1) l) (!res)
    in
    prf.(0) <- 0;
    for i = 1 to n do
        prf.(i) <- arr.(i-1) + prf.(i-1)
    done;
    memoize tab dp n k

