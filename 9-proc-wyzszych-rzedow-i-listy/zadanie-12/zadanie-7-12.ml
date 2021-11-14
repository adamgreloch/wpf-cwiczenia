(* Dana jest lista liczb całkowitych. Element tej listy nazwiemy prextremum
jeśli jest ostro większy lub ostro mniejszy od wszystkich elementów
poprzedzających go na danej liście.  Napisz procedurę
prextrema : int list → int list
która dla danej listy zwraca listę wszystkich jej prextremów.  Elementy na
liście wynikowej powinny być w takiej samej kolejności, w jakiej występowały na
danej liście. Na przykład:
prextrema [-2; 1; 0; 1; 3; 2; -1; 5; 4; -3; 2; 1; 7] = [-2; 1; 3; 5; -3; 7]. *)

open List

let prextrema l =
    let helper (res, x_min, x_max) x =
        if x > x_max || x < x_min then
            (x::res, min x_min x, max x_max x)
        else
            (res, x_min, x_max)
    in
    let (r,_,_) = fold_left helper ([hd l], hd l, hd l) (tl l) in
    rev r
;;

