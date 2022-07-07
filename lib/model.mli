(*type model_label
module Session :
sig
  type session
  and resp
  val dual : session -> session
  val dualResp : resp -> resp
end
type binding_acc
type bindings
*)