open Printf
open Dict

(* max x direction bound for map *)
let xbound : int = 50
(* max y direction bound for map*)
let ybound : int = 50

(* representing direction north, south, east and west *)
type dir = N | S | E | W

(* record type representing an npc *)
type npc = {
  name : string;
  id : int;
  occupation : string option;
  knowledge : string option;
  relation : float;
  status : float;
  (* TODO: task machine type *)
} 

(* record type representing objects that can be picked up, sold, used, etc. *)
type obj = {
  name : string;
  (* TODO: add logic for items *)
}

(* record type representing a shop that can sell objects for a given price *)
type shop = {
  name : string;
  id : int;
  items : obj * int Seq.t;
  owner : npc;
}

(* constructor type representing various actions that can be done by the player character*)
type action = Fight of npc (* TODO: add env variable here *)
            | Pickup of obj

(* record type representing the options available to player in a room *)
type options = {
  npcs : npc Seq.t;
  shops : shop Seq.t;
  actions : action Seq.t;
  subrooms : options option;
  room : options option;
}

(* lazy datatype wrapped in a thunk based on direction input.
this represents the map that will be traversed by the player *)
type city = Stream of (dir -> options * city)

module IntOrder : Dict.ORDERED with type t = int = struct
  type t = int
  let compare (x, y) = if x < y then LESS else (if x > y then GREATER else EQUAL)
end

module T = Dict.RBTDict (IntOrder)
module I = Dict.IRBTDict (T)
let mapDict : options I.dict = I.empty ()


let rec mkCity (x, y) = Stream (function d ->
  match d with
    N -> printf "(%d, %d)" x (y + 1); (getOptions (x, y + 1), mkCity (x, y + 1))
  | S -> printf "(%d, %d)" x (y - 1); (getOptions (x, y - 1), mkCity (x, y + 1))
  | E -> printf "(%d, %d)" (x + 1) y; (getOptions (x + 1, y), mkCity (x + 1, y))
  | W -> printf "(%d, %d)" (x - 1) y; (getOptions (x - 1, y), mkCity (x - 1, y))
)


