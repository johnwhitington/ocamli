type f = {mutable s : unit -> unit; mutable n : unit -> unit}

let d y () = y.s ()

let mf =
  let x = {s = (fun _ -> ()); n = (fun () -> ())} in
    x.n <- d x;
    x
