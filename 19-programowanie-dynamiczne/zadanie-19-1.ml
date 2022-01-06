(* 1. Antyczny bardzo długi kijek został połamany na kawałki. Należy go skleić.
Wszystkie kawałki zostały już ułożone w odpowiedniej kolejności, ale nie
wiadomo ile potrzeba kleju. Długości kolejnych kawałków są dane w postaci listy
l = [x1; x2; ...; xn]. Do sklejenia kawałków długości a i b potrzeba max(a, b)
ml kleju. Po ich sklejeniu otrzymujemy jeden kawałek długości a + b.
Napisz procedurę klej : int list -> int, która na podstawie długości kawałków
obliczy minimalną ilość kleju (w ml) potrzebną do sklejenia wszystkich
kawałków. *)

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

(* top-down jest bez sensu, bo mało podproblemów się pokrywa, zatem ma okropną
   złożoność *)
let klej_td lst =
    let tab = ref Map.empty in
    let sklej x y l =
        let rec pom some = function
            | [] -> []
            | h::hy::t when h = x -> (List.rev ((x+y)::some)) @ t
            | h::t -> pom (h::some) t
        in
        pom [] l
    in
    let rec f = function
        | [] -> 0
        | x::[] -> 0
        | l ->
        let res = ref (max_int/2)
        and arr = Array.of_list l in
        for i = 0 to Array.length arr - 2 do
            res := min !res (max arr.(i) arr.(i+1) + f (sklej arr.(i) arr.(i+1) l)) 
        done;
        !res
    in
    memoize tab f lst

(* spróbujmy napisać bottom-up
   optymalna podstruktura: każde sklejenie danego podproblemu ma być optymalne *)

(* let sklej i l =
    let rec pom curr = function
        | [] -> []
        | h::t -> if curr = i then
            match t with
            | [] -> [h]
            | hc::tc -> (h+hc)::tc
        else
            h::(pom (curr+1) t)
    in
    pom 0 l
*)

(* T(n) = O(n^2) M(n) = O(n) *)
let klej lst =
    let kawalki = ref lst
    and n = List.length lst in
    let koszty = Array.make (n-1) (max_int, 0)
    and t = ref 0 in (* t := liczba wykonanych sklejeń *)
    let sklej i l =
        let rec pom curr = function
            | [] -> []
            | h::t -> if curr = i then
                match t with
                | [] -> [h]
                | hc::tc -> (h+hc)::tc
            else
                h::(pom (curr+1) t)
        in
        pom 0 l
    in
    while n - !t > 1 do (* n - !t to pozostale kawalki do sklejenia *)
        let arr = Array.of_list !kawalki in
        for i = 0 to n - !t - 2 do
            if max arr.(i) arr.(i+1) < fst koszty.(!t) then
                koszty.(!t) <- (max arr.(i) arr.(i+1), i);
        done;
        (* Printf.printf "%d %d %d koszt: %d\n" !rem arr.(snd koszty.(!t))
        arr.(snd koszty.(!t)+1) (fst koszty.(!t)); *)
        kawalki := sklej (snd koszty.(!t)) !kawalki;
        t := !t + 1;
    done;
    Array.fold_left (fun sum (a,_) -> a + sum) 0 koszty

