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

type bindings_t = { acc: binding_map; cond_acc: binding_map }

let recvCallback (binding : BindingRef.t) x : Session.Types.session =
  Recv (binding.cb, x)

(*module SThunk = struct

  type inner =
    | KnownS of Session.RespSet.t
    | ThunkS of BindingRef.t * inner

  type t = InContext of bindings_t * inner

  let apply bindings ~binding cont =
    let thunk = ThunkS (binding, KnownS cont) in
    InContext (bindings, thunk)

  let force (InContext (bindings, x)) =
    match x with
    | KnownS set ->
      set
    | ThunkS (x, (ThunkS (y, _) as innermost)) as outermost ->
      if x == y
      then forceThunk bindings innermost
      else forceThunk bindings outermost
    | _ -> assert false

  let rec forceThunk bindings (ThunkS (binding, arg)) =
    let SnC
    let get_acc x = Derivation.respFor bindings ~binding:x
    let acc = List.map 

end*)

let (<<) f g x = f (g x)

let rec respFor bindings ~binding (cont : Session.RespSet.t Lazy.t) =
  let cond = BindingRefMap.find binding bindings.cond_acc in
  let specific_cont x = next bindings ~binding ~cond x in
  let recv = Lazy.map (recvCallback binding << specific_cont) cont in
  (EventOccured binding), recv

and next bindings ~binding ~cond (cont : Session.RespSet.t) =
  match cond with
  | h :: t ->
    let head = (respFor bindings ~binding:h (lazy cont)) in
    let cont2 = Session.RespSet.add head cont in
    let case_fun = next bindings ~binding ~cond:t in
    let local_label x = CondAddBinding (x, h, binding) in
    let responses =
      [ local_label true, lazy (case_fun cont2)
      ; local_label false, lazy (case_fun cont) ]
    in
    Session.Types.Branch (Session.RespSet.of_list responses)
  | [] -> assert false
    (*let s_thunk = SThunk.apply bindings ~binding cont in
    Select (Session.RespSet.map dualResp @@ SThunk.force s_thunk)*)
