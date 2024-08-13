type ordered = Dict.ordered = LESS | EQUAL | GREATER

module IntOrder : Dict.ORDERED with type t = int = struct
  type t = int
  let compare (x, y) = if x < y then LESS else (if x > y then GREATER else EQUAL)
end

module T = Dict.RBTDict (IntOrder)
module I = Dict.IRBTDict (T)

let test : string I.dict = I.empty ()
let () = I.insert (5, "hi") test
let x : string option = I.find 5 test

let y = match x with None -> "" | Some a -> a

let () = Printf.printf "%s\n" y