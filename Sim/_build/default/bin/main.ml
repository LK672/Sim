type ordered = Dict.ordered = LESS | EQUAL | GREATER

module IntOrder : Dict.ORDERED with type t = int = struct
  type t = int
  let compare (x, y) = if x < y then LESS else (if x > y then GREATER else EQUAL)
end

module TreeDict = Dict.RBTDict (IntOrder)

module I = Dict.IRBTDict (TreeDict)

let d = I.empty ()
let () = I.insert (5, "hi") d


let () = print_endline "Success"