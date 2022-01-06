(* ELEVTRBL - Elevator Trouble *)

let elev f s g u d =
    let q = ref (Queue.create())
    and min_presses = Array.make (f+1) (-1) in
    let is_good x = x >= 1 && x <= f && min_presses.(x) = (-1) in
    let bfs v =
        Queue.add (v,0) !q;
        while not (Queue.is_empty !q) do
            let (x,p) = Queue.pop !q in
            min_presses.(x) <- p;
            if is_good (x+u) then
                Queue.add (x+u,p+1) !q;
            if is_good (x-d) then
                Queue.add (x-d,p+1) !q;
        done
    in
    if (s < g && u = 0) || (s > g && d = 0) then
        Printf.printf "use the stairs"
    else (
        bfs s;
        if min_presses.(g) = -1 then
            Printf.printf "use the stairs"
        else
            Printf.printf "%d" min_presses.(g)
    )
;;

Scanf.scanf "%d %d %d %d %d" elev
