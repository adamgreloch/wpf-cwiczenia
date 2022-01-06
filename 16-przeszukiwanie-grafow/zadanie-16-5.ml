(* [XI OI, zadanie Wyspy, mod]
Dana jest prostokątna mapa n×m przedstawiająca archipelag na oceanie,
reprezentowana jako `bool array array`. Elementy tablicy równe `true`
reprezentują ląd, a `false` wodę. Na zewnątrz mapy znajduje się ocean.

Wyspy na oceanie są wyspami rzędu 1. Na wyspach rzędu 1 mogą znajdować się
jeziora rzędu 1, na nich mogą znajdować się wyspy rzędu 2 itd.
Ogólnie:

        ocean ma rząd 0,
        wyspa na jeziorze (lub oceanie) rzędu k , ma rząd k+1,
        jezioro na wyspie rzędu k ma rząd k.

Pola wody przylegające do siebie bokami lub rogami należą do tego samego
jeziora (lub oceanu). Pola lądu przylegające do siebie bokami należą do tej
samej wyspy.

Napisz procedurę `wyspa:bool array array -> int`, która dla danej mapy zwróci
maksymalny rząd wyspy na mapie. Jeżeli na oceanie nie ma żadnej wyspy, to
procedura powinna zwrócić 0.

Przykład: 69.793N, 108.241W. *)


(* kod z https://github.com/kmichael08/wpf-OCaml/blob/master/zadanka/wyspa.ml *)

let wyspa map =
    let n = Array.length map and m = Array.length map.(0) in
    let mapa = Array.make_matrix (n + 2) (m + 2) false in
    for i = 1 to n do
        for j = 1 to m do
            mapa.(i).(j) <- map.(i - 1).(j - 1) 
        done
    done;
    let kol = ref (Queue.create())
    and ruchy_lad x y = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] in
    let ruchy_woda x y = 
        (ruchy_lad x y) @ [(x + 1, y + 1); (x + 1, y - 1); (x - 1, y + 1); (x - 1, y - 1)] 
    and maks = ref 0 and ranga = Array.make_matrix (n + 2) (m + 2) (-1) in
    let is_good a b = a >= 0 && b >= 0 && a < n + 2 && b < m + 2 && ranga.(a).(b) = -1 in
    let bfs waga (x, y) =
        let ruchy = if mapa.(x).(y) then ruchy_lad else ruchy_woda in
            Queue.add (x, y) !kol;
            while not (Queue.is_empty !kol) do
                let (a, b) = Queue.pop !kol in
                    ranga.(a).(b) <- waga;
                    List.iter (fun (c, d) -> if is_good c d && mapa.(c).(d) = mapa.(a).(b) then
                                             Queue.add (c, d) !kol) (ruchy a b)
            done
    in bfs 0 (0, 0);
    for i = 1 to n + 1 do
        for j = 1 to m + 1 do
            if ranga.(i).(j) = -1 then
                begin
                    let waga = if mapa.(i).(j) then ranga.(i - 1).(j) + 1
                    else ranga.(i - 1).(j) in
                    bfs waga (i, j);
                    maks := max !maks waga
                end
        done
    done;
    !maks

(* 
----###----
--#####----
-#----##---
-#-##-###--
-#----###--
--#######--
------##---
*)
