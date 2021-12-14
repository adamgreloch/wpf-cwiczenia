(*
Miejska komisja epidemiologiczna odkryła, że podczas pewnego przejazdu jednego
z autobusów doszło do sporej liczby zakażeń nieznanym wirusem. Aby lepiej
zbadać sprawę, komisja potrzebuje wiedzieć dla każdego pasażera z iloma innymi
pasażerami miał on styczność. Dla każdego pasażera wiemy na którym przystanku
wsiadł i wysiadł. Pasażerowie mieli ze sobą styczność, gdy byli razem w
autobusie pomiędzy jakimiś dwoma przystankami.

Napisz funkcję wirus : (int * int) list -> int list, która dla listy wejściowej
[(a_1 , b_1); ...; (a_n , b_n)] zwróci listę [s_1 ; ...; s_n], gdzie i-ty
pasażer wsiadł na przystanku a_i, wysiadł na przystanku b_i, gdzie b_i > a_i, a
s_i oznacza, że miał styczność z s_i pasażerami. Przykład:

wirus [(2,5);(4,7);(1,8);(3,4);(4,5);(8,9);(6,7)] = [4; 4; 5; 2; 3; 0; 2]
*)

(* T(n) = M(n) = O(n) *)

(* Tworzę dwie listy posortowane odpowiednio po czasie wejścia do autobusu i
   wyjścia. Na podstawie tych list tworzę dwie tablice sum prefiksowych
   zliczających liczbę pasażerów którzy wsiedli do i-tego przystanku włącznie
   oraz tych którzy wysiedli. Tablice te pozwalają na ustalenie w czasie stałym
   do ilu pasażerów dołącza dopiero wchodzący pasażer na [a]-tym przystanku
   (preIn.(a) - preOut.(a) - 1) oraz ustelenie ilu pasażerów dołączy jeszcze do
   podróży od następnego przystanku [a+1] do przystanku końcowego [b] danego
   pasażera, dając w sumie liczbę pasażerów z którymi miał styczność. *)

let wirus pas =
    let pas_a = List.sort (fun (a1,_) (a2,_) -> compare a1 a2) pas
    and pas_b = List.sort (fun (_,b1) (_,b2) -> compare b1 b2) pas
    in
    let n = List.fold_left (fun acc (_,b) -> max acc b + 1) 0 pas_b in
    let preIn = Array.make n 0 and preOut = Array.make n 0
    in
    let rec fillPre arr f i = function
        | [] -> arr.(i+1) <- arr.(i);
        | h::t as l -> (
            if i = f h then (
                if arr.(i) = 0 then
                    arr.(i) <- arr.(i-1) + 1
                else
                    arr.(i) <- arr.(i) + 1
                ;
                fillPre arr f i t
            ) else (
                if arr.(i) = 0 then arr.(i) <- arr.(i-1);
                fillPre arr f (i+1) l
            )
        )
    in
    begin
        fillPre preIn fst 1 pas_a;
        fillPre preOut snd 1 pas_b;
        List.map (fun (a,b) ->
            preIn.(a) - preOut.(a) - 1 + preIn.(b-1) - preIn.(a)) pas
    end
;;

wirus [(2,5);(4,7);(1,8);(3,4);(4,5);(8,9);(6,7)]
