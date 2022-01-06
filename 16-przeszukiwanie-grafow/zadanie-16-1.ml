(* Dany jest graf nieskierowany, którego wierzchołki są ponumerowane
od 0 do n − 1. Napisz procedurę path : graph -> int, która wyznacza długość
(liczoną liczbą krawędzi) najdłuższej ścieżki w tym grafie, na której numery
wierzchołków tworzą ciąg rosnący. *)

module type GRAPH_LABELS = sig
  type label
end

module type GRAPH = sig
    (* Etykiety krawędzi. *)
    type label
    (* Typ grafów o wierzchołkach typu int. *)
    type graph
    (* Pusty graf. *)
    val init: int -> graph
    (* Rozmiar grafu *)
    val size: graph -> int
    (* wierzchołki są numerowane od 0 do size-1 *)
    (* Dodanie krawędzi skierowanej łączącej dwa wierzchołki. *)
    val insert_directed_edge: graph -> int -> int -> label -> unit
    (* Dodanie krawędzi nieskierowanej (dwukierunkowej) łączącej dwa wierzchołki. *)
    val insert_edge: graph -> int -> int -> label -> unit
    (* Lista incydencji danego wierzchołka. *)
    val neighbours: graph -> int -> (int * label) list
end

module Graph = functor (L : GRAPH_LABELS) ->
    (struct
    (* Etykiety krawędzi. *)
    type label = L.label
    (* Typ grafów - tablica list sąsiedztwa. *)
    type graph = {n : int; e : (int*label) list array}
    (* Pusty graf. *)
    let init s = {n = s; e = Array.make s []}
    (* Rozmiar grafu. *)
    let size g = g.n
    (* Dodanie krawędzi skierowanej łączącej dwa wierzchołki. *)
    let insert_directed_edge g x y l =
        assert ((x<>y) && (x >= 0) && (x <size g) && (y >= 0) && (y <size g) &&
        (List.filter (fun (v,_) -> v=y) g.e.(x) = []));
        g.e.(x) <- (y,l)::g.e.(x)
    (* Dodanie krawędzi łączącej dwa (istniejące) wierzchołki. *)
    let insert_edge g x y l =
        insert_directed_edge g x y l;
        insert_directed_edge g y x l
    (* Lista incydencji danego wierzchołka. *)
    let neighbours g x =
        g.e.(x)
    end : GRAPH with type label = L.label);;

module G : GRAPH with type label = unit = Graph(struct type label = unit end) 

let path g =
    let n = G.size g in
    let max_path = Array.make n (-1) in
    let rec dfs v =
        if max_path.(v) >= 0 then
            max_path.(v)
        else
            let answ = ref 0 in
            begin
                max_path.(v) <- 0;
                List.iter (fun (u,_) ->
                    if u > v then answ := max !answ (dfs u)) (G.neighbours g v);
                answ := !answ + 1;
                max_path.(v) <- !answ;
                !answ
            end
    in
    let i = ref 0 in
    let answ = ref 0 in
    begin
        while !i < n do
            answ := max (dfs !i) !answ;
            i := !i + 1;
        done;
        !answ
    end

