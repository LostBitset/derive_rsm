open Derive_rsm;;

let aux1 = Events.BindingRef.of_int 1 in
let example : Model.bindings_t =
  { acc = (Model.BindingRefMap.add aux1 [])
            (Model.BindingRefMap.add Events.BindingRef.top [] @@ Model.BindingRefMap.empty)
  ; cond_acc = (Model.BindingRefMap.add aux1 [])
                 (Model.BindingRefMap.add Events.BindingRef.top [aux1] @@ Model.BindingRefMap.empty) }
in
let res = Model.runtimeSession example in
let list1 =
  match res with
  | Model.Session.Types.Select set -> Model.Session.RespSet.elements set
  | _ -> failwith "not select with 1 element"
in
let forced1 =
  match list1 with
  | [(_, x)] -> Lazy.force x
  | _ -> failwith "not list of one 2-tuple"
in
print_endline "reached end"
