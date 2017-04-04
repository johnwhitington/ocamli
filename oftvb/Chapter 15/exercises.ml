let rec concat l =
  match l with
    [] -> []
  | h::t -> h @ concat t

let rec concat_tail a l =
  match l with
    [] -> List.rev a
  | h::t -> concat_tail (List.rev h @ a) t

let concat l =
  concat_tail [] l

let all_contain_true l =
  not (List.mem false (List.map (List.mem true) l))

let count_exclamations s =
  let n = ref 0 in
    String.iter (function '!' -> n := !n + 1 | _ -> ()) s;
    !n

let calm =
  String.map (function '!' -> '.' | x -> x)

let concat =
  String.concat ""

let concat ss =
  let b = Buffer.create 100 in
    List.iter (Buffer.add_string b) ss;
    Buffer.contents b

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
