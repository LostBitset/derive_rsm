module Make (Msg : sig
    type t
    type label
    val compare_labels : label -> label -> int
  end) = struct

  module rec Types : sig
    type session =
      | Send of Msg.t * session
      | Recv of Msg.t * session
      | Branch of RespSet.t 
      | Select of RespSet.t
    and resp = Msg.label * session Lazy.t
  end = Types
  
  and RespSet : Set.S with type elt = Types.resp = Set.Make(struct
    
    type t = Types.resp

    let compare (x, _ : t) (y, _ : t) = Msg.compare_labels x y

  end)

  open Types;;

  let rec dual = function
    | Send (x, n) -> Recv (x, n)
    | Recv (x, n) -> Send (x, n)
    | Branch xs -> Select (RespSet.map dualResp xs)
    | Select xs -> Branch (RespSet.map dualResp xs)

  and dualResp (l, next) = (l, Lazy.map dual next)

end
