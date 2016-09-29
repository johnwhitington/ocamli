module type STATE = sig val x : int end

module State : STATE = struct let x = 1 end

let y = State.x

