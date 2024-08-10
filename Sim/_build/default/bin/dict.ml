type ordered = LESS | EQUAL | GREATER

(* signature for ordered types *)
module type ORDERED = sig
  type t (* abstract *)
  val compare : t * t -> ordered (* element comparison function *)
end

(* signature for dictionaries *)
module type DICT = sig
  module Key : ORDERED  (* parameter *)
  type 'a entry = Key.t * 'a  (* concrete *)
  type 'a dict  (* abstract *)

  val empty : 'a dict
  val find : Key.t -> 'a dict -> 'a option
  val insert : 'a entry -> 'a dict -> 'a dict
end

(* functor to make Red-Black Tree implemented dictionaries *)
module RBTDict (K : ORDERED) : (DICT with type Key.t = K.t) = struct
  module Key = K 
  type 'a entry = Key.t * 'a
  type 'a rbt = Empty | Red of 'a rbt * 'a * 'a rbt | Black of 'a rbt * 'a * 'a rbt
  type 'a dict = 'a entry rbt

  let empty = Empty

  let rec find (k' : Key.t) (t : 'a dict) =
    match t with
      Empty -> None
    | Red (l, (k, v), r) -> 
        (match Key.compare (k', k) with
          EQUAL -> Some v
        | LESS -> find k' l
        | GREATER -> find k' r)
    | Black (l, (k, v), r) ->
        (match Key.compare (k', k) with
          EQUAL -> Some v
        | LESS -> find k' l
        | GREATER -> find k' r)

  type 'a almost =
      OK of 'a rbt
    | BadL of ('a rbt * 'a * 'a rbt) * 'a * 'a rbt  (* INVARIANT: all three rbts are black with the same black height *)
    | BadR of 'a rbt * 'a * ('a rbt * 'a * 'a rbt)  (* INVARIANT: all three rbts are black with the same black height *)

  (* Submodule to encapsulate ins and recolor *)
  module Internal = struct
    let rec ins ((k', v') : 'a entry) (t : 'a entry rbt) : 'a entry almost =
      match t with
        Empty -> OK (Red (Empty, (k', v'), Empty))
      | Red (l, (k, v), r) ->
          (match Key.compare (k', k) with
            EQUAL -> OK (Red (l, (k', v'), r))
          | LESS ->
              (match ins (k', v') l with
              OK (Red (dl, dx, dr)) -> BadL ((dl, dx, dr), (k, v), r)
            | OK l' -> OK (Red (l', (k, v), r))
            | _ -> raise (Failure "impossible by ENSURES")
              )
          | GREATER ->
              (match ins (k', v') r with
                OK (Red (dl, dx, dr)) -> BadR (l, (k, v), (dl, dx, dr))
              | OK r' -> OK (Red (l, (k, v), r'))
              | _ -> raise (Failure "impossible by REQUIRES")
              )
          )
      | Black (l, (k, v), r) ->
          OK (
            match Key.compare (k', k) with
              EQUAL -> Black (l, (k, v), r)
            | LESS -> 
                (match ins (k', v') l with
                  OK l' -> Black (l', (k, v), r)
                | BadL ((t1, x, t2), y, t3) ->
                    Red (Black (t1, x, t2), y, Black (t3, (k, v), r))
                | BadR (t1, x, (t2, y, t3)) ->
                    Red (Black (t1, x, t2), y, Black (t3, (k, v), r))
                )
            | GREATER ->
                (match ins (k', v') r with
                  OK r' -> Black (l, (k, v), r')
                | BadL ((t1, x, t2), y, t3) ->
                    Red (Black (l, (k, v), t1), x, Black (t2, y, t3))
                | BadR (t1, x, (t2, y, t3)) ->
                    Red (Black (l, (k, v), t1), x, Black (t2, y, t3))
                )
          )

    let recolor (a : 'a almost) =
      match a with
        OK t -> t
      | BadL ((d1l, d1x, d1r), y, t2) -> Black (Red (d1l, d1x, d1r), y, t2)
      | BadR (t1, x, (d2l, d2x, d2r)) -> Black (t1, x, Red (d2l, d2x, d2r))
  end
  
  (* Insert function uses the internal functions *)
  let insert (k, v) t = Internal.recolor (Internal.ins (k, v) t)
end 

(* signature for imperative dictionaries *)
module type IDICT = sig
  module Key : ORDERED
  type 'a entry (* abstract *)
  type 'a dict  (* abstract *)

  val empty : unit -> 'a dict (* We need to create a ref cell so we will make it a function *)
  val find : Key.t -> 'a dict -> 'a option
  val insert : 'a entry -> 'a dict -> unit (* since we are just updating a ref cell no need to return anything meaningful *)
end 

(* functor that takes in a dictionary structure and produces an imperative dictionary *)
module IRBTDict (T : DICT) : (IDICT) with type 'a entry = 'a T.entry = struct
  module Key = T.Key
  type 'a entry = 'a T.entry
  type 'a dict = 'a T.dict ref

  let empty = function () -> ref T.empty
  let find (k' : Key.t) (t : 'a dict) = T.find k' !t
  let insert (k, v) (t : 'a dict) = t := T.insert (k, v) !t; ()
end


