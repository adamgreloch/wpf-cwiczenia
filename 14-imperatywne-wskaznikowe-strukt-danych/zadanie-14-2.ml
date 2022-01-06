(*
Dana jest definicja typu elementów tworzących listy wskaźnikowe:
type 'a option = None | Some of 'a
type 'a elem = {v: 'a; mutable next: 'a lista}
and 'a lista = 'a elem option
(a) Napisz procedurę petla : lista → unit, która mając daną listę jednokierunkową,
tworzy z niej listę cykliczną, ale z odwróconym kierunkiem wskaźników. Możesz
założyć, że dana lista jest poprawną listą jednokierunkową, to znaczy ma koniec i
nie zapętla się.
(b) Napisz procedurę przeplot : lista → lista → unit, która splata obie listy w
jedną listę postaci: pierwszy rekord pierwszej listy, pierwszy rekord drugiej listy,
drugi rekord pierwszej listy, drugi rekord drugiej listy, itd. Jeśli jedna z list jest
dłuższa, to jej końcówka stanowi końcówkę listy wynikowej.
*)

type 'a option = None | Some of 'a

type 'a elem = {v: 'a; mutable next: 'a lista}
and 'a lista = 'a elem option

let petla l =
    match l with
    | None -> ()
    | Some (el) ->
        let last = ref el in
        let next = ref el.next in 
        while !next <> None do
            match !next with
            | None -> ()
            | Some (nextV)->
                next := nextV.next;
                nextV.next <- Some(!last);
                last := nextV;
        done;
        el.next <- Some(!last)

let przeplot lista1 lista2 = 
    let k = ref 0 in
    let l1 = ref lista1 in
    let l2 = ref lista2 in
    
    while !l1 <> None && !l2 <> None do
        match (!l1, !l2, !k) with
        | (Some (a), Some (b), 0) -> 
            begin
                let n = a.next in
                a.next <- Some (b);
                l1 := n; 
                k := 1 - !k;
            end
        | (Some (a), Some (b), 1) ->
            begin
                let n = b.next in
                b.next <- Some (a);
                l2 := n; 
                k := 1 - !k;
            end
    done;
;;

