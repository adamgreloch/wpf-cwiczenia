(* Dany jest typ reprezentujący drzewa binarne: *)

type 'a tree = Node of 'a tree * 'a * 'a tree | Null;;

(* Zdefiniuj typ α przechadzka oraz procedury:

buduj: α tree → α przechadzka
w_lewo: α przechadzka → α przechadzka
w_prawo: α przechadzka → α przechadzka
w_gore: α przechadzka → α przechadzka
obejrzyj: α przechadzka → α

umożliwiające „przechadzanie” się po drzewie i oglądanie go. Wywołanie buduj d
powinno konstruować przechadzkę, w której znajdujemy się w korzeniu drzewa d.
Wywołanie w_lewo p powinno konstruować przechadzkę powstałą przez przejście
do lewego poddrzewa. Analogicznie powinny działać procedury w_prawo i w_gore.
Procedura obejrzyj powinna zwrócić element przechowywany w wierzchołku drzewa,
w którym aktualnie jesteśmy.

Podczas przechadzki po drzewie możemy przebywać jedynie w wierzchołkach Node _.
Jeśli nastąpi próba wejścia do pustego drzewa Null lub wyjścia „ponad” korzeń,
należy podnieść wyjątek Poza_drzewem. *)

let drzewo1 = Node(Null,1,Node(Null,2,Null));;
let drzewo2 = Node(Null,1,Node(Null,2,Null));;
let drzewo3 = Node(drzewo2,1,Node(Null,3,Node(Null,4,drzewo2)));;
let drzewo4 = Node(drzewo2,1,Node(Null,3,drzewo2));;
let drzewo5 = Node(drzewo2,1,Node(Null,3,Node(Null,1,drzewo2)));;

module Chodzimy_se =
    struct
        open List

        type 'a przechadzka = 'a tree list

        exception Poza_drzewem

        let buduj tree = [tree]

        let w_lewo p = match hd p with
            | Null -> raise Poza_drzewem
            | Node(l,_,_) when l = Null -> raise Poza_drzewem
            | Node(l,_,_) -> l::p

        let w_prawo p = match hd p with
            | Null -> raise Poza_drzewem
            | Node(_,_,r) when r = Null -> raise Poza_drzewem
            | Node(_,_,r) -> r::p

        let w_gore p = match tl p with
            | [] -> raise Poza_drzewem
            | l -> l

        let obejrzyj p =
            match hd p with
            | Null -> raise Poza_drzewem
            | Node(l,x,r) -> x
    end
;;

open Chodzimy_se
