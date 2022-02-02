open List

let gwozdzie lst =
    let srt = sort (fun x y -> compare (snd x) (snd y)) lst in
    let (a,b) = hd srt
    and gwozdzie = ref 0 in
    let rec aux prev x = function
        | [] -> ()
        | (p,q)::[] ->
                Printf.printf "wbij końcowy x=%d\n" x;
                gwozdzie := !gwozdzie + 1;
        | (p,q)::t ->
                if p <= x then
                    (Printf.printf "maksymalizacja dla p=%d\n" p;
                    aux q x t)
                else begin
                    Printf.printf "wbij x=%d\n" prev;
                    gwozdzie := !gwozdzie + 1;
                    aux q (fst (hd t)) t
                end
    in
    aux b a (tl srt);
    !gwozdzie
;;

gwozdzie [(1,3);(2,3);(5,6);(1,10);(1,2)];;
gwozdzie [(7,14);(1,2);(2,3);(3,4);(4,5);(5,6);(3,6);(1,6);(6,7)];;
