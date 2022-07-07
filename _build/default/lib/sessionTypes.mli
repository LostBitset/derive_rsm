module Make :
  functor (_ : sig
    type t
    type label
    val compare_labels : label -> label -> int
  end) ->
    sig
      type session and resp
      val dual : session -> session
      val dualResp : resp -> resp
    end
