module type FIND_UNION = sig
    (* Typ klas elementów typu 'a. *)
    type 'a set

    (* Tworzy nową klasę złożoną tylko z danego elementu. *)
    val make_set : 'a -> 'a set

    (* Znajduje reprezentanta danej klasy. *)
    val find : 'a set -> 'a

    (* Sprawdza, czy dwa elementy są równoważne. *)
    val equivalent : 'a set -> 'a set -> bool 

    (* Scala dwie dane (rozłączne) klasy. *)
    val union : 'a set -> 'a set -> unit

    (* Lista elementów klasy. *)
    val elements : 'a set -> 'a list

    (* Liczba wszystkich klas. *)
    val n_of_sets : unit-> int
end;;

module FU : FIND_UNION = struct
    (* Typ klas elementów typu 'a. *)
    type 'a set = { 
        elem         : 'a;                   (* Element klasy. *)
        up           : 'a set ref;   (* Przodek w drzewie find-union. *)
        mutable rank : int;                  (* Ranga w drzewie find-union. *)
        mutable next : 'a set list   (* Lista potomków w pewnym drzewie rozpinającym klasę. *)
    }

    (* Licznik klas. *)
    let sets_counter = ref 0

    (* Liczba wszystkich klas. *)
    let n_of_sets () = !sets_counter

    (* Tworzy nową klasę złożoną tylko z danego elementu. *)
    let make_set x = 
        let rec v = { elem = x; up = ref v; rank = 0; next = [] }
        in begin
            sets_counter := !sets_counter + 1;
            v
        end

    (* Znajduje korzeń drzewa, kompresując ścieżkę. *)
    let rec go_up s = 
        if s == !(s.up) then s 
            else begin
                s.up := go_up !(s.up);
                !(s.up)
            end

    (* Znajduje reprezentanta danej klasy. *)
    let find s = 
        (go_up s).elem
    
    (* Sprawdza, czy dwa elementy są równoważne. *)
    let equivalent s1 s2 =
        go_up s1 == go_up s2

    (* Scala dwie dane (rozłączne) klasy. *)
    let union x y = 
        let fx = go_up x 
        and fy = go_up y
        in
        if not (fx == fy) then begin
            if fy.rank > fx.rank then begin
                fx.up := fy;
                fy.next <- fx :: fy.next
            end else begin
                fy.up := fx;
                fx.next <- fy :: fx.next;
                if fx.rank = fy.rank then fx.rank <- fy.rank + 1
            end;
        sets_counter := !sets_counter - 1
        end
    
    (* Lista elementów klasy. *)
    let elements s = 
        let acc = ref []
        in
        let rec traverse s1 = 
            begin
                acc := s1.elem :: !acc;
                List.iter traverse s1.next
            end
        in begin
            traverse (go_up s);
            !acc
        end
end

(* T(m*n) = Theta(m*n+(m*n+l)log*(m*n)) = Theta((m*n+l)log^*(m*n))
   l = Length.lst
   ale l ~ m*n więc
   T(m*n) = Theta((m*n)log^*(m*n)) *)

let labirynt m n lst =
    let a = FU.make_set (0,0) in
    let arr = Array.make_matrix m n a in
    let rec aux l prev res = 
        match l with
        | [] -> res
        | ((x,y,o) as h)::t ->
            let (px,py) = if o then (x,y-1) else (x-1,y) in
            FU.union arr.(x).(y) arr.(px).(py);
            if FU.n_of_sets () = prev then
                aux t prev (h::res) 
            else
                aux t (FU.n_of_sets ()) res
    in
    for y = 1 to n - 1 do
        arr.(0).(y) <- FU.make_set (0,y);
    done;
    for x = 1 to m - 1 do
        arr.(x).(0) <- FU.make_set (x,0);
        for y = 1 to n - 1 do
            arr.(x).(y) <- FU.make_set (x,y);
        done
    done;
    aux (List.rev lst) (n*m) []
;;

labirynt 3 2 [(2,1,false); (2,1,true); (2,0,false); (1,1,true);
(0,1,true); (1,1,false); (1,0,false) ]
