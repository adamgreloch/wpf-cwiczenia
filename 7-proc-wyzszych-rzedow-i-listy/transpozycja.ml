open List

let macierz = [[1;2;3];[4;5;6];[7;8;9]]

let rec trans l =
    fold_right (fun h t -> map2 (fun a b -> a::b) h t) l (map (fun _ -> []) l)
;;
