let occurrences ss s =
  if ss = "" then 0 else
    let num = ref 0 in
      let str = ref s in
        while
          String.length ss <= String.length !str && !str <> ""
        do
          if String.sub !str 0 (String.length ss) = ss then
            num := !num + 1;
          str := String.sub !str 1 (String.length !str - 1)
        done;
        !num
