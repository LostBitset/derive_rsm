(*open Events

type model_label =
  | EventOccured of BindingRef.t
  | CondAddBinding of bool * BindingRef.t * BindingRef.t

module Session = SessionTypes.Make(struct
    type t = callback_ref
    type label = model_label
  end)

type binding_acc = BindingRef.t * BindingRef.t list

type bindings = { acc: binding_acc; cond_acc: binding_acc }*)
