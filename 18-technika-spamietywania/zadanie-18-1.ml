(* 1. Jaka jest minimalna liczba monet lub banknotów potrzebna do wydania n zł
reszty, przyjmując, że w obrocie są dostępne monety o zadanych (w postaci
listy) nominałach? *)

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

let reszta n nom =
    let a = Array.of_list nom
    and tab = ref Map.empty
    in
    let rec f m = 
        if m < 0 then max_int/2 else
        if m = 0 then 0 else
        let res = ref max_int in
        for i = 0 to Array.length a - 1 do
            res := min !res (1 + f (m - a.(i)))
        done;
        !res
    in
    match memoize tab f n with
    | answ when answ > max_int/2 -> -1
    | answ -> answ

