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

let rec respFor bindings ~binding cont : Session.Types.resp =
  let cond = BindingRefMap.find binding bindings in
  let specific_cont = lazy (next bindings ~binding ~cond cont) in
  let recv = Lazy.map (recvCallback binding) specific_cont in
  (EventOccured binding), recv

and next bindings ~binding ~cond cont =
  match cond with
  | h :: t ->
    let cont_prime = Session.RespSet.add (respFor bindings ~binding:h cont) cont in
    let case_fun = next bindings ~binding ~cond:t in
    let local_label x = CondAddBinding (x, h, binding) in
    let responses =
      [ local_label true, lazy (case_fun cont_prime)
      ; local_label false, lazy (case_fun cont) ]
    in
    Session.Types.Branch (Session.RespSet.of_list responses)
  | [] -> assert false
