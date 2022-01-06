(* [Zadanie 3 ze smurfa ćw 19] Jaka jest minimalna liczba monet potrzebna do
wydania n zł reszty, przyjmując, że monety mogą być przekazywane w obie strony.
(Na przykład, żeby otrzymać 9 zł mogę dać 1 zł i otrzymać 10 zł, razem 2). W
obrocie są dostępne monety o zadanych (w postaci tablicy) nominałach. *)

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
