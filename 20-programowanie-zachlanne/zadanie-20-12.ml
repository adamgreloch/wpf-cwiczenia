let wpf lst =
    let lst_wsp = List.sort (fun (_,_,a) (_,_,b) -> compare a b) (List.map (fun (t,p) -> (t,p,p/t)) lst)
    in
    let rec aux l res z = 
        match l with
        | [] -> res
        | (t,p,_)::tl -> aux tl (res+t*z+p) (z+p)
    in
    aux lst_wsp 0 0
