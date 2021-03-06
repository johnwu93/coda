(* TODO: check if this is needed *)
open Core_kernel
open Coda_base
open Coda_state

module type S = sig
  type t = {state: Protocol_state.Value.t; proof: Proof.Stable.V1.t}
  [@@deriving bin_io, fields, sexp]

  val create : state:Protocol_state.Value.t -> proof:Proof.Stable.V1.t -> t
end

include S
