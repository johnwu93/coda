[%%import
"../../../config.mlh"]

open Core
open Async
open Coda_worker
open Coda_base
open Coda_inputs
open Pipe_lib

type t = Coda_worker.Connection.t * Process.t * Coda_worker.Input.t

let spawn_exn (config : Coda_worker.Input.t) =
  let%bind conn, process =
    Coda_worker.spawn_in_foreground_exn ~env:config.env ~on_failure:Error.raise
      ~cd:config.program_dir ~shutdown_on:Disconnect
      ~connection_state_init_arg:() ~connection_timeout:(Time.Span.of_sec 15.)
      config
  in
  File_system.dup_stdout process ;
  File_system.dup_stderr process ;
  return (conn, process, config)

let local_config ?proposal_interval ~peers ~addrs_and_ports ~acceptable_delay
    ~program_dir ~proposer ~snark_worker_config ~work_selection ~offset
    ~trace_dir ~max_concurrent_connections () =
  let conf_dir =
    Filename.temp_dir_name
    ^/ String.init 16 ~f:(fun _ -> (Int.to_string (Random.int 10)).[0])
  in
  let config =
    { Coda_worker.Input.addrs_and_ports
    ; env=
        ( "CODA_TIME_OFFSET"
        , Time.Span.to_int63_seconds_round_down_exn offset
          |> Int63.to_int
          |> Option.value_exn ?here:None ?message:None ?error:None
          |> Int.to_string )
        :: ( Core.Unix.environment () |> Array.to_list
           |> List.filter_map
                ~f:
                  (Fn.compose
                     (function [a; b] -> Some (a, b) | _ -> None)
                     (String.split ~on:'=')) )
    ; proposer
    ; snark_worker_config
    ; work_selection
    ; peers
    ; conf_dir
    ; trace_dir
    ; program_dir
    ; acceptable_delay
    ; max_concurrent_connections }
  in
  config

let disconnect (conn, proc, _) =
  let%bind () = Coda_worker.Connection.close conn in
  let%map (_ : Unix.Exit_or_signal.t) = Process.wait proc in
  ()

let peers_exn (conn, proc, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.peers ~arg:()

let get_balance_exn (conn, proc, _) pk =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.get_balance
    ~arg:pk

let get_nonce_exn (conn, proc, _) pk =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.get_nonce
    ~arg:pk

let root_length_exn (conn, proc, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.root_length
    ~arg:()

let send_user_command_exn (conn, proc, _) sk pk amount fee memo =
  Coda_worker.Connection.run_exn conn
    ~f:Coda_worker.functions.send_user_command
    ~arg:(sk, pk, amount, fee, memo)

let process_user_command_exn (conn, proc, _) cmd =
  Coda_worker.Connection.run_exn conn
    ~f:Coda_worker.functions.process_user_command ~arg:cmd

let prove_receipt_exn (conn, proc, _) proving_receipt resulting_receipt =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.prove_receipt
    ~arg:(proving_receipt, resulting_receipt)

let sync_status_exn (conn, proc, _) =
  let%map r =
    Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.sync_status
      ~arg:()
  in
  Linear_pipe.wrap_reader r

let verified_transitions_exn (conn, proc, _) =
  let%map r =
    Coda_worker.Connection.run_exn conn
      ~f:Coda_worker.functions.verified_transitions ~arg:()
  in
  Linear_pipe.wrap_reader r

let new_block_exn (conn, proc, __) key =
  let%map r =
    Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.new_block
      ~arg:key
  in
  Linear_pipe.wrap_reader r

let root_diff_exn (conn, proc, _) =
  let%map r =
    Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.root_diff
      ~arg:()
  in
  Linear_pipe.wrap_reader r

let start_exn (conn, proc, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.start ~arg:()

let new_user_command_exn (conn, proc, _) pk =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.new_user_command
    ~arg:pk

let get_all_user_commands_exn (conn, proc, _) pk =
  Coda_worker.Connection.run_exn conn
    ~f:Coda_worker.functions.get_all_user_commands ~arg:pk

let dump_tf (conn, proc, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.dump_tf ~arg:()

let best_path (conn, proc, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.best_path
    ~arg:()
