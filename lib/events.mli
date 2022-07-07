type event
type callback_ref
module BindingRef :
  sig
    type t = { ev : event; cb : callback_ref; }
    val compare : t -> t -> int
    val top : t
    val of_int : int -> t
  end
