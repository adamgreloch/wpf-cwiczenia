open Array

let przestaw a =
    let n = length a in
    let i = ref 0
    and j = ref 0
    and k = ref (n-1) in
    let neg = make n 0
    and nonneg = make n 0
    in
    begin
        while !i < n && a.(!i) < 0 do
            neg.(!i) <- a.(!i);
            i := !i + 1
        done;
        while !j + !i < n do
            nonneg.(!j) <- a.(!j + !i);
            j := !j + 1;
        done;
        j := !j - 1;
        i := 0;
        while !j >= 0 && !k >= 0 && !i < n do
            if abs(neg.(!i)) > nonneg.(!j) then
                begin
                    a.(!k) <- neg.(!i);
                    i := !i + 1
                end
            else
                begin
                    a.(!k) <- nonneg.(!j);
                    j := !j - 1
                end;
            k := !k - 1;
        done;
    end

