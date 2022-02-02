(* 1. [PCh, Zadanie o misjonarzach i kanibalach] Przez rzekę chcą przeprawić
się kanibale i misjonarze. Kanibali i misjonarzy jest po trzech. Mają jedną
łódkę, którą może płynąć do dwóch osób. Łódką umieją wiosłować kanibale i tylko
jeden misjonarz.  Jeżeli w dowolnej chwili, na dowolnym brzegu rzeki będzie
więcej kanibali, niż misjonarzy, to zjedzą oni misjonarzy. W jaki sposób
wszyscy mogą bezpiecznie przeprawić się na drugi brzeg. *)

(* ((m1,p1,k1),(mł,pł,kł),(m2,p2,k2))
   m - zwykli misjonarze
   p - powerful misjonarze
   k - kanibale *)

let misjonarze =
    let q = ref (Queue.create())
    and visited = Hashtbl.create 424242 in
    let is_good cfg = m1 >= && m2 >= um
    in
    let moves ((m1,u1,k1),(ml,ul,kl),(m2,u2,k2)) =
        [((m1-1,u1,k1),(ml+1,ul,kl),(m2,u2,k2))
    

