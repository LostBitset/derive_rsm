type event = EventId of int

type callback_ref = CallbackId of int

module BindingRef = struct

  type t = { ev: event; cb: callback_ref }

  let compare = compare 

  let top = { ev = EventId 0; cb = CallbackId 0 }

  let of_int n = { ev = EventId n; cb = CallbackId n }

end
