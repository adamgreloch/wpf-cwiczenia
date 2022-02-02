(* 2. Ustawić minimalną liczbę hetmanów atakujących wszystkie pozostałe pola na
szachownicy. *)

let debug = false

exception Solution of (int * int) list
exception NoSolution

let pd s =
    if debug then
        Printf.printf s

let pd2 s a b =
    if debug then
        Printf.printf s a b

let pd3 s a b c =
    if debug then
        Printf.printf s a b c

let rec print_list = function
    | [] -> ()
    | (a,b)::t -> begin
        if debug then
        Printf.printf "(%d,%d), " a b;
        print_list t
    end

(* ekstremalny brute-force na który straciłem cały dzień. Nauczka na
   przyszłość: najpierw myśleć, potem pisać kod.

   Żeby backtracking miał tu sens, trzeba znaleźć jakikolwiek warunek *)
let hetman n =
    let visited = Hashtbl.create 42424242
    and q = ref (Queue.create())
    in
    let is_good (a,b) = 0 <= a && a < n && 0 <= b && b < n in
    let all_covered cfg =
        let chb = Array.make_matrix n n 1 in
        let attack a b =
            if is_good (a,b) then begin
                pd2 "(%d).(%d) <- 0 \n" (a) (b);
                chb.(a).(b) <- 0
            end
        in
        print_list cfg;
        pd "\n";
        List.iter (fun (px,py) ->
            for i = 0 to n-1 do
                pd3 "px=%d py=%d i=%d \n" px py i;
                attack (px+i) (py+i);
                attack (px-i) (py-i);
                attack (px+i) (py-i);
                attack (px-i) (py+i);
                attack (i) (py);
                attack (px) (i)
            done) cfg;
        Array.for_all (fun x -> Array.for_all (fun y -> y = 0) x) chb
    in
    (*let is_new cfg =
        not (List.exists (fun l -> Hashtbl.mem visited (List.sort compare l)) (
        List.fold_left (fun acc f -> (List.map f cfg)::acc)
        [] [(fun (i,j) -> (i,j));
            (fun (i,j) -> (n-1-j,i));
            (fun (i,j) -> (n-1-i,n-1-j));
            (fun (i,j) -> (j,n-1-i));
            (fun (i,j) -> (n-1-j,n-1-i));
            (fun (i,j) -> (j,i));
            (fun (i,j) -> (n-1-i,j));
            (fun (i,j) -> (i,n-1-j))]))
    in
    let attacked (x0,y0) =
        List.fold_left (fun res (x1,y1) -> x0 = x1 || y0 = y1 || abs (x0 - x1)
        = abs (y0 - y1)) false 
    in*)
    let add e cfg =
        if is_good e then
        let new_cfg = (List.sort compare (e::cfg)) in
        if not (List.mem e cfg) && not (Hashtbl.mem visited new_cfg) then
            Queue.add new_cfg !q;
            Hashtbl.add visited new_cfg true;
    in
    let gen cfg =
        for i = 0 to n-1 do
            for j = 0 to n-1 do
                add (i,j) cfg;
                    (*for k = 0 to n-1 do
                        add (i+k,j+k) cfg;
                        add (i-k,j-k) cfg;
                        add (i+k,j-k) cfg;
                        add (i-k,j+k) cfg;
                        add (i,k) cfg;
                        add (k,j) cfg;
                    done
                end*)
            done
        done
    in
    gen [];
    try
    (while not (Queue.is_empty !q) do
        let cfg = Queue.pop !q in
        if all_covered cfg then
            raise (Solution cfg)
        else gen cfg
    done;
    raise NoSolution)
    with Solution x -> x;;
