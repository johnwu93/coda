open Core
open Async
open Nanobit_base
open Blockchain_snark
open Cli_common

module Inputs = struct
  module Time = Block_time
  module Hash = struct
    type 'a t = Snark_params.Tick.Pedersen.Digest.t
    [@@deriving sexp]
    (* TODO *)
    let hash _ = Snark_params.Tick.Pedersen.zero_hash
  end
  module Transaction = struct
    type 'a t =
      { transaction: Nanobit_base.Transaction.t
      ; validation: 'a
      }

    let of_transaction transaction =
      { transaction
      ; validation = `Unknown
      }

    let check t =
      if Nanobit_base.Transaction.check_signature t.transaction then
        Some { t with validation = `Valid_signature }
      else
        None
  end
  module Nonce = Nanobit_base.Nonce
  module Difficulty = struct
    type t = Target.t

    let next t ~last ~this =
      Blockchain_state.compute_target last t this
  end
  module Strength = struct
    type t = Strength.t

    (* TODO *)
    let increase t ~by = t
  end
  module Ledger = struct
    type t = Nanobit_base.Ledger.t
  end
  module Ledger_proof = struct
    type t = unit
    type input = Ledger.t Hash.t

    (* TODO *)
    let verify t _ = return true
  end
  module Transition = struct
    type t =
      { ledger_hash : Ledger.t Hash.t
      ; ledger_proof : Ledger_proof.t
      ; timestamp : Time.t
      ; nonce : Nonce.t
      }
    [@@deriving fields]
  end
  module Time_close_validator = struct
    let validate t =
      let now = Time.of_time (Core_kernel.Time.now ()) in
      Time.(diff now t < (Span.of_time_span (Core_kernel.Time.Span.of_sec 900.)))
  end
  module State = struct
    type t =
      { next_difficulty      : Difficulty.t
      ; previous_state_hash  : t Hash.t
      ; ledger_hash          : Ledger.t Hash.t
      ; strength             : Strength.t
      ; timestamp            : Time.t
      }
    [@@deriving fields]

    module Proof = struct
      type input = t
      type t = unit

      (* TODO *)
      let verify t _ = return true
    end
  end
  module Proof_carrying_state = struct
    type t = (State.t, State.Proof.t) Protocols.Minibit_pow.Proof_carrying_data.t
  end
  module State_with_witness = struct
    type 'a witness = 'a Transaction.t list
    type 'a t =
      { transactions : 'a witness
      ; state : Proof_carrying_state.t
      }

    let forget_witness {state} = state
    (* TODO should we also consume a ledger here so we know the transactions valid? *)
    let add_witness_exn state transactions =
      {state ; transactions}
    (* TODO same *)
    let add_witness state transactions = Or_error.return {state ; transactions}
  end
  module Transition_with_witness = struct
    type 'a witness = 'a Transaction.t list
    type 'a t =
      { transactions : 'a witness
      ; transition : Transition.t
      }

    let forget_witness {transition} = transition
    (* TODO should we also consume a ledger here so we know the transactions valid? *)
    let add_witness_exn transition transactions =
      {transition ; transactions}
    (* TODO same *)
    let add_witness transition transactions = Or_error.return {transition ; transactions}
  end
  module State_io = struct
    type t = unit

    let create ~broadcast_state = ()

    let new_states t =
      let (r,w) = Linear_pipe.create () in
      r
  end
  module Ledger_fetcher = struct
    type t = unit
    let create ~ledger_transitions = ()

    let get t hash = Deferred.never ()
  end
  module Transaction_pool = struct
    type t

    (* TODO *)
    let add t valid_transactions = t
    let remove t valid_transactions = t
    let get t ~k  = []
  end
  module Miner = struct
    type t = ()
    type change =
      | Tip_change of State.t

    let create ~change_feeder = ()

    let transitions t =
      let (r, w) = Linear_pipe.create () in
      r
  end
  module Genesis = struct
    (* TODO actually do this right *)
    let state : State.t =
      { next_difficulty = Blockchain_state.zero.Blockchain_state.target
      ; previous_state_hash = Blockchain_state.negative_one.Blockchain_state.block_hash
      ; ledger_hash = Snark_params.Tick.Field.of_int 0
      ; strength = Nanobit_base.Strength.zero
      ; timestamp = Block.genesis.Block.header.Block.Header.time
      }
    let proof = ()
  end
  module Block_state_transition_proof = struct
    module Witness = struct
      type t =
        { old_state : State.t
        ; old_proof : State.Proof.t
        ; transition : Transition.t
        }
    end

    (* TODO *)
    let prove_zk_state_valid t ~new_state = return ()
  end
end

module Main = Minibit.Make(Inputs)(Inputs.Block_state_transition_proof)


let daemon =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Current daemon"
    begin
      [%map_open
        let conf_dir =
          flag "config directory"
            ~doc:"Configuration directory"
            (optional file)
        and should_mine =
          flag "mine"
            ~doc:"Run the miner" (required bool)
        and port =
          flag "port"
            ~doc:"Server port for other to connect" (required int16)
        and ip =
          flag "ip"
            ~doc:"External IP address for others to connect" (optional string)
        and start_prover =
          flag "start-prover" no_arg
            ~doc:"Start a new prover process"
        and prover_port =
          flag "prover-port" (optional_with_default Prover.default_port int16)
            ~doc:"Port for prover to listen on" 
        in
        fun () ->
          let open Deferred.Let_syntax in
          let%bind home = Sys.home_directory () in
          let conf_dir =
            Option.value ~default:(home ^/ ".current-config") conf_dir
          in
          let%bind () = Unix.mkdir ~p:() conf_dir in
          let minibit = Main.create () in
          printf "Created minibit\n%!";
          Main.run minibit;
          printf "Ran minibit\n%!";
          Async.never ()

          (*let%bind initial_peers =*)
            (*let peers_path = conf_dir ^/ "peers" in*)
            (*match%bind Reader.load_sexp peers_path [%of_sexp: Host_and_port.t list] with*)
            (*| Ok ls -> return ls*)
            (*| Error e -> *)
              (*begin*)
                (*let default_initial_peers = [] in*)
                (*let%map () = Writer.save_sexp peers_path ([%sexp_of: Host_and_port.t list] default_initial_peers) in*)
                (*[]*)
              (*end*)
          (*in*)
          (*let%bind prover =*)
            (*if start_prover*)
            (*then Prover.create ~port:prover_port ~debug:()*)
            (*else Prover.connect { host = "0.0.0.0"; port = prover_port }*)
          (*in*)
          (*let%bind genesis_proof = Prover.genesis_proof prover >>| Or_error.ok_exn in*)
          (*let genesis_blockchain =*)
            (*{ Blockchain.state = Blockchain.State.zero*)
            (*; proof = genesis_proof*)
            (*; most_recent_block = Block.genesis*)
            (*}*)
          (*in*)
          (*let%bind () = Main.assert_chain_verifies prover genesis_blockchain in*)
          (*let%bind ip =*)
            (*match ip with*)
            (*| None -> Find_ip.find ()*)
            (*| Some ip -> return ip*)
          (*in*)
          (*let minibit = Main.create ()*)
          (*let log = Logger.create () in*)
          (*Main.main*)
            (*~log*)
            (*~prover*)
            (*~storage_location:(conf_dir ^/ "storage")*)
            (*~genesis_blockchain*)
            (*~initial_peers *)
            (*~should_mine*)
            (*~me:(Host_and_port.create ~host:ip ~port)*)
            (*()*)
      ]
    end
;;

  let () = 
  Command.group ~summary:"Current"
    [ "daemon", daemon
    ; "prover", Prover.command
    ; "rpc", Main_rpc.command
    ; "client", Client.command
    ]
  |> Command.run
;;

let () = never_returns (Scheduler.go ())
;;
