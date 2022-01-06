open Array

let niemalejacy a =
    let i = ref 0 in
    try
        begin
            while !i < length a - 1 do
                begin
                if (abs(a.(!i+1)) - abs(a.(!i))) < 0 then
                    raise Exit;
                i := !i + 1
                end
            done;
            true
        end
    with Exit -> false
;;
