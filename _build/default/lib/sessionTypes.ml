module Make (Msg : sig
    type t
    type label
    val compare_labels : label -> label -> int
  end) = struct

  module RespSet(Msg : sig
      type label
      val compare_labels : label -> label -> int
    end) = Set.Make(struct
      type ignored
      type t = Msg.label * ignored
      let compare (a, _ : t) (b, _ : t) = Msg.compare_labels a b
    end)

  type session =
    | Send of Msg.t * session
    | Recv of Msg.t * session
    | Branch of RespSet(Msg).t
    | Select of resp list

  and resp = Msg.label * session Lazy.t

  let rec dual = function
    | Send (x, n) -> Recv (x, n)
    | Recv (x, n) -> Send (x, n)
    | Branch xs -> Select (List.map dualResp xs)
    | Select xs -> Branch (List.map dualResp xs)

  and dualResp (l, next) = (l, Lazy.map dual next)

end
