type f = { mutable s : string -> int -> int -> unit; mutable n : unit -> unit; }

let d state () = state.s "\n" 0  1 (*removing this fixes the problem *)

let f1 f h = { s = f; n = h; }

let output a b c d = ()

let osubstring a b c d = ()

let mf output flush =
  let ppf = f1 output ignore in
  ppf.n <- d ppf;
  ppf

let foc oc = mf (osubstring oc) (fun () -> ())

let sf = foc stdout
