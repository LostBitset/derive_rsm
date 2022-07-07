open Events

type model_label =
  | EventOccured of BindingRef.t
  | CondAddBinding of bool * BindingRef.t * BindingRef.t

module Session = SessionTypes.Make(struct

    type t = callback_ref

    type label = model_label

    let compare_labels = compare

  end)

module BindingRefMap = Map.Make(BindingRef)

type binding_map = BindingRef.t list BindingRefMap.t

type bindings = { acc: binding_map; cond_acc: binding_map }

let recvCallback (binding : BindingRef.t) x =
  Session.Types.Recv (binding.cb, x)

let respFor bindings ~binding cont : Session.Types.resp =
  let cond = BindingRefMap.find binding bindings in
  let specificCont = lazy (next bindings ~binding ~cond cont) in
  let recv = Lazy.map (recvCallback binding) specificCont in
  (EventOccured binding), recv

let next bindings ~binding ~cond cont =
  match cond with
  | 
