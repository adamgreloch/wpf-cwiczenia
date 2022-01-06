(* 2. [II OI, zadanie Klub Prawoskrętnych Kierowców, PCh] Mamy dany układ ulic,
leżących na prostokątnej siatce m × n. Ulice są jedno- lub dwukierunkowe.
Przyjmujemy, że cztery strony świata są reprezentowane za pomocą liczb
całkowitych od 0 do 3: *)

let north = 0 and east = 1 and south = 2 and west = 3;;

(* Układ ulic jest dany w formie trójwymiarowej tablicy wartości logicznych
ulice: ulice.(i).(j).(k) określa, czy z punktu o współrzednych (i, j) można
przejechać w kierunku k jedną jednostkę odległości. Można założyć, że tablica
ta uniemożliwia wyjechanie poza obszar {0, . . . , m − 1} × {0, . . . , n − 1}.

Członkowie Klubu Prawoskrętnych Kierowców na skrzyżowaniach jeżdżą prosto lub
skręcają w prawo. Sprawdź, czy z punktu (0, 0) prawoskrętny kierowca może
dojechać do punktu (m − 1, n − 1). *)

let kpk tbl =
    let m = Array.length tbl.(0)
    and n = Array.length tbl in
    let visited = Array.make_matrix m n 0
    and q = ref (Queue.create()) in
    let move_from (x,y) o =
        if x < m && y < n && tbl.(x).(y).(o) && visited.(x).(y) < 3 then begin
            let (nx,ny) = match o with
            | 0 -> (x,y+1)
            | 1 -> (x+1,y)
            | 2 -> (x,y-1)
            | _ -> (x-1,y)
            in
            Queue.add ((nx,ny), o) !q;
            visited.(nx).(ny) <- visited.(nx).(ny) + 1
        end
    in
    move_from (0,0) east;
    move_from (0,0) north;
    while not (Queue.is_empty !q) do
        let ((x,y), o) = Queue.pop !q in
        let l = match o with
        | 0 -> [north;east]
        | 1 -> [east;north]
        | 2 -> [south;west]
        | _ -> [west;north]
        in
        List.iter (move_from (x,y)) l
    done;
    visited.(m-1).(n-1) > 0

