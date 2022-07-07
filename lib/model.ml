open Events;;

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

module type S_THUNK = sig
  type t
  val apply : bindings_t -> binding:BindingRef.t -> Session.RespSet.t -> t
  val force : t -> Session.RespSet.t
end

module type DERIVATION = sig
  val respFor :
    bindings_t
    -> binding:BindingRef.t
    -> Session.RespSet.t Lazy.t
    -> Session.Types.resp
end

module rec SThunk : S_THUNK = struct

  type inner =
    | KnownS of Session.RespSet.t
    | ThunkS of BindingRef.t * inner

  type t = InContext of bindings_t * inner

  let apply bindings ~binding cont =
    let thunk = ThunkS (binding, KnownS cont) in
    InContext (bindings, thunk)

  let rec force (InContext (bindings, x)) =
    match x with
    | KnownS set ->
      set
    | ThunkS (x, (ThunkS (y, _) as innermost)) as outermost ->
      if x == y
      then force @@ InContext (bindings, innermost)
      else forceInner bindings outermost
    | _ as noIdempotency ->
      forceInner bindings noIdempotency

  and forceInner bindings x =
    match x with
    | ThunkS (binding, arg) ->
      let cont = lazy (force @@ InContext (bindings, arg)) in
      let get_acc x = Derivation.respFor bindings ~binding:x cont in
      let acc_list = binding :: BindingRefMap.find binding bindings.acc in
      let acc = List.map get_acc acc_list in
      let acc_set = Session.RespSet.of_list acc in
      Session.RespSet.union acc_set @@ Lazy.force cont
    | KnownS set ->
      set

end

and Derivation : DERIVATION = struct

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
    | [] ->
      let s_thunk = SThunk.apply bindings ~binding cont in
      Select (Session.RespSet.map Session.dualResp @@ SThunk.force s_thunk)

end

let runtimeSession bindings =
  let empty = lazy Session.RespSet.empty in
  let topResp = Derivation.respFor bindings ~binding:BindingRef.top empty in
  Session.Types.Select (Session.RespSet.singleton topResp)

let test = 
  let aux1 = Events.BindingRef.of_int 1 in
  let example : bindings_t =
    { acc = (BindingRefMap.add aux1 [])
              (BindingRefMap.singleton Events.BindingRef.top [])
    ; cond_acc = (BindingRefMap.add aux1 [])
                  (BindingRefMap.singleton Events.BindingRef.top [aux1]) }
  in
  let res = runtimeSession example in
  let list1 =
    match res with
    | Session.Types.Select set -> Session.RespSet.elements set
    | _ -> failwith "not select with 1 element"
  in
  let forced1 =
    match list1 with
    | [(_, x)] -> Lazy.force x
    | _ -> failwith "not list of one 2-tuple"
  in
  forced1
