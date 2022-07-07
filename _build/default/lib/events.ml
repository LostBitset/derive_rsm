type event = EventId of int

type callback_ref = CallbackId of int

module BindingRef = struct

  type t = { ev: event; cb: callback_ref }

  let pairing = function
    | { ev = EventId x; cb = CallbackId y } ->
      Pairing.cantor x y

  let on f g x y = f (g x) (g y)

  let compare = on compare pairing

  let top = { ev = EventId 0; cb = CallbackId 0 }

end
