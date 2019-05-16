[%%import
"../../../config.mlh"]

open Core
open Async
open Coda_base
open Coda_state
open Coda_inputs
open Signature_lib
open Pipe_lib
open O1trace

let pk_of_sk sk = Public_key.of_private_key_exn sk |> Public_key.compress

[%%if
proof_level = "full"]

let with_snark = true

[%%else]

let with_snark = false

[%%endif]

let run_test () : unit Deferred.t =
  Parallel.init_master () ;
  let logger = Logger.create () in
  File_system.with_temp_dir "full_test_config" ~f:(fun temp_conf_dir ->
      let keypair = Genesis_ledger.largest_account_keypair_exn () in
      let module Config = struct
        let logger = logger

        let conf_dir = temp_conf_dir

        let lbc_tree_max_depth = `Finite 50

        let propose_keypair = Some keypair

        let genesis_proof = Precomputed_values.base_proof

        let commit_id = None

        let work_selection = Protocols.Coda_pow.Work_selection.Seq
      end in
      let%bind (module Init) =
        make_init ~should_propose:true (module Config)
      in
      let%bind () =
        match Unix.getenv "CODA_TRACING" with
        | Some trace_dir ->
            let%bind () = Async.Unix.mkdir ~p:() trace_dir in
            Coda_tracing.start trace_dir
        | None ->
            Deferred.unit
      in
      let module Main = Coda_inputs.Make_coda (Init) in
      let module Run = Coda_run.Make (Config) (Main) in
      let trace_database_initialization typ location =
        Logger.trace logger "Creating %s at %s" ~module_:__MODULE__ ~location
          typ
      in
      let%bind trust_dir = Async.Unix.mkdtemp (temp_conf_dir ^/ "trust_db") in
      let trust_system = Trust_system.create ~db_dir:trust_dir in
      trace_database_initialization "trust_system" __LOC__ trust_dir ;
      let%bind receipt_chain_dir_name =
        Async.Unix.mkdtemp (temp_conf_dir ^/ "receipt_chain")
      in
      trace_database_initialization "receipt_chain_database" __LOC__
        receipt_chain_dir_name ;
      let receipt_chain_database =
        Coda_base.Receipt_chain_database.create
          ~directory:receipt_chain_dir_name
      in
      let%bind transaction_database_dir =
        Async.Unix.mkdtemp (temp_conf_dir ^/ "transaction_database")
      in
      trace_database_initialization "transaction_database" __LOC__
        receipt_chain_dir_name ;
      let transaction_database =
        Transaction_database.create logger transaction_database_dir
      in
      let time_controller = Main.Inputs.Time.Controller.(create basic) in
      let consensus_local_state =
        Consensus.Data.Local_state.create
          (Some (Public_key.compress keypair.public_key))
      in
      let net_config =
        Main.Inputs.Net.Config.
          { logger
          ; trust_system
          ; time_controller
          ; consensus_local_state
          ; gossip_net_params=
              { timeout= Time.Span.of_sec 3.
              ; logger
              ; target_peer_count= 8
              ; initial_peers= []
              ; conf_dir= temp_conf_dir
              ; addrs_and_ports=
                  { external_ip= Unix.Inet_addr.localhost
                  ; bind_ip= Unix.Inet_addr.localhost
                  ; discovery_port= 8001
                  ; communication_port= 8000 }
              ; trust_system
              ; max_concurrent_connections= Some 10 } }
      in
      Core.Backtrace.elide := false ;
      Async.Scheduler.set_record_backtraces true ;
      let largest_account_keypair =
        Genesis_ledger.largest_account_keypair_exn ()
      in
      let run_snark_worker =
        `With_public_key
          (Public_key.compress largest_account_keypair.public_key)
      in
      let%bind coda =
        Main.create
          (Main.Config.make ~logger ~trust_system ~net_config
             ~propose_keypair:keypair
             ~snark_worker_key:
               (Public_key.compress largest_account_keypair.public_key)
             ~transaction_pool_disk_location:
               (temp_conf_dir ^/ "transaction_pool")
             ~snark_pool_disk_location:(temp_conf_dir ^/ "snark_pool")
             ~wallets_disk_location:(temp_conf_dir ^/ "wallets")
             ~time_controller ~receipt_chain_database
             ~snark_work_fee:(Currency.Fee.of_int 0) ~consensus_local_state
             ~transaction_database ())
      in
      Main.start coda ;
      don't_wait_for
        (Strict_pipe.Reader.iter_without_pushback
           (Main.verified_transitions coda)
           ~f:ignore) ;
      let wait_until_cond ~(f : Main.t -> bool) ~(timeout : Float.t) =
        let rec go () =
          if f coda then return ()
          else
            let%bind () = after (Time.Span.of_sec 10.) in
            go ()
        in
        Deferred.any [after (Time.Span.of_min timeout); go ()]
      in
      let balance_change_or_timeout ~initial_receiver_balance receiver_pk =
        let cond t =
          match
            Run.Commands.get_balance t receiver_pk
            |> Participating_state.active_exn
          with
          | Some b when not (Currency.Balance.equal b initial_receiver_balance)
            ->
              true
          | _ ->
              false
        in
        wait_until_cond ~f:cond ~timeout:3.
      in
      let assert_balance pk amount =
        match
          Run.Commands.get_balance coda pk |> Participating_state.active_exn
        with
        | Some balance ->
            if not (Currency.Balance.equal balance amount) then
              failwithf
                !"Balance in account (%{sexp: Public_key.Compressed.t}) \
                  %{sexp: Currency.Balance.t} is not asserted balance %{sexp: \
                  Currency.Balance.t}"
                pk balance amount ()
        | None ->
            failwith
              (sprintf !"Invalid Account: %{sexp: Public_key.Compressed.t}" pk)
      in
      let client_port = 8123 in
      Run.setup_local_server ~client_port ~coda () ;
      Run.run_snark_worker ~client_port run_snark_worker ;
      (* Let the system settle *)
      let%bind () = Async.after (Time.Span.of_ms 100.) in
      (* No proof emitted by the parallel scan at the begining *)
      assert (Option.is_none @@ Run.For_tests.ledger_proof coda) ;
      (* Note: This is much less than half of the high balance account so we can test
       *       payment replays being prohibited
      *)
      let send_amount = Currency.Amount.of_int 10 in
      (* Send money to someone *)
      let build_payment amount sender_sk receiver_pk fee =
        trace_recurring_task "build_payment" (fun () ->
            let nonce =
              Run.Commands.get_nonce coda (pk_of_sk sender_sk)
              |> Participating_state.active_exn
              |> Option.value_exn ?here:None ?error:None ?message:None
            in
            let payload : User_command.Payload.t =
              User_command.Payload.create ~fee ~nonce
                ~memo:User_command_memo.dummy
                ~body:(Payment {receiver= receiver_pk; amount})
            in
            User_command.sign (Keypair.of_private_key_exn sender_sk) payload )
      in
      let assert_ok x = assert (Or_error.is_ok x) in
      let test_sending_payment sender_sk receiver_pk =
        let payment =
          build_payment send_amount sender_sk receiver_pk
            (Currency.Fee.of_int 0)
        in
        let prev_sender_balance =
          Run.Commands.get_balance coda (pk_of_sk sender_sk)
          |> Participating_state.active_exn
          |> Option.value_exn ?here:None ?error:None ?message:None
        in
        let prev_receiver_balance =
          Run.Commands.get_balance coda receiver_pk
          |> Participating_state.active_exn
          |> Option.value ~default:Currency.Balance.zero
        in
        let%bind p1_res =
          Run.Commands.send_user_command coda (payment :> User_command.t)
        in
        assert_ok (p1_res |> Participating_state.active_exn) ;
        (* Send a similar payment twice on purpose; this second one will be rejected
           because the nonce is wrong *)
        let payment' =
          build_payment send_amount sender_sk receiver_pk
            (Currency.Fee.of_int 0)
        in
        let%bind p2_res =
          Run.Commands.send_user_command coda (payment' :> User_command.t)
        in
        assert_ok (p2_res |> Participating_state.active_exn) ;
        (* The payment fails, but the rpc command doesn't indicate that because that
           failure comes from the network. *)
        (* Let the system settle, mine some blocks *)
        let%map () =
          balance_change_or_timeout
            ~initial_receiver_balance:prev_receiver_balance receiver_pk
        in
        assert_balance receiver_pk
          ( Currency.Balance.( + ) prev_receiver_balance send_amount
          |> Option.value_exn ?here:None ?error:None ?message:None ) ;
        assert_balance (pk_of_sk sender_sk)
          ( Currency.Balance.( - ) prev_sender_balance send_amount
          |> Option.value_exn ?here:None ?error:None ?message:None )
      in
      let send_payment_update_balance_sheet sender_sk sender_pk receiver_pk
          amount balance_sheet fee =
        let payment = build_payment amount sender_sk receiver_pk fee in
        let new_balance_sheet =
          Map.update balance_sheet sender_pk (fun v ->
              Option.value_exn
                (Currency.Balance.sub_amount (Option.value_exn v)
                   (Option.value_exn (Currency.Amount.add_fee amount fee))) )
        in
        let new_balance_sheet' =
          Map.update new_balance_sheet receiver_pk (fun v ->
              Option.value_exn
                (Currency.Balance.add_amount (Option.value_exn v) amount) )
        in
        let%map p_res =
          Run.Commands.send_user_command coda (payment :> User_command.t)
        in
        p_res |> Participating_state.active_exn |> assert_ok ;
        new_balance_sheet'
      in
      let send_payments accounts pks balance_sheet f_amount =
        Deferred.List.foldi accounts ~init:balance_sheet
          ~f:(fun i acc ((keypair : Signature_lib.Keypair.t), _) ->
            let sender_pk = Public_key.compress keypair.public_key in
            let receiver =
              List.random_element_exn
                (List.filter pks ~f:(fun pk -> not (pk = sender_pk)))
            in
            send_payment_update_balance_sheet keypair.private_key sender_pk
              receiver (f_amount i) acc (Currency.Fee.of_int 0) )
      in
      let block_count t =
        Run.best_protocol_state t |> Participating_state.active_exn
        |> Protocol_state.consensus_state
        |> Consensus.Data.Consensus_state.length
      in
      let wait_for_proof_or_timeout timeout () =
        let cond t = Option.is_some @@ Run.For_tests.ledger_proof t in
        wait_until_cond ~f:cond ~timeout
      in
      let test_multiple_payments accounts pks timeout =
        let balance_sheet =
          Public_key.Compressed.Map.of_alist_exn
            (List.map accounts
               ~f:(fun ((keypair : Signature_lib.Keypair.t), account) ->
                 ( Public_key.compress keypair.public_key
                 , account.Account.Poly.balance ) ))
        in
        let%bind updated_balance_sheet =
          send_payments accounts pks balance_sheet (fun i ->
              Currency.Amount.of_int ((i + 1) * 10) )
        in
        (*After mining a few blocks and emitting a ledger_proof (by the parallel scan), check if the balances match *)
        let%map () = wait_for_proof_or_timeout timeout () in
        assert (Option.is_some @@ Run.For_tests.ledger_proof coda) ;
        Map.fold updated_balance_sheet ~init:() ~f:(fun ~key ~data () ->
            assert_balance key data ) ;
        block_count coda
      in
      let test_duplicate_payments (sender_keypair : Signature_lib.Keypair.t)
          (receiver_keypair : Signature_lib.Keypair.t) =
        let%bind () =
          test_sending_payment sender_keypair.private_key
            (Public_key.compress receiver_keypair.public_key)
        in
        test_sending_payment sender_keypair.private_key
          (Public_key.compress receiver_keypair.public_key)
      in
      let pks accounts =
        List.map accounts ~f:(fun ((keypair : Signature_lib.Keypair.t), _) ->
            Public_key.compress keypair.public_key )
      in
      (*Need some accounts from the genesis ledger to test payment replays and
        sending multiple payments*)
      let receiver_keypair =
        let receiver =
          Genesis_ledger.find_new_account_record_exn
            [largest_account_keypair.public_key]
        in
        Genesis_ledger.keypair_of_account_record_exn receiver
      in
      let sender_keypair =
        let sender =
          Genesis_ledger.find_new_account_record_exn
            [largest_account_keypair.public_key; receiver_keypair.public_key]
        in
        Genesis_ledger.keypair_of_account_record_exn sender
      in
      let other_accounts =
        List.filter Genesis_ledger.accounts ~f:(fun (_, account) ->
            let reserved_public_keys =
              [ largest_account_keypair.public_key
              ; receiver_keypair.public_key
              ; sender_keypair.public_key ]
            in
            not
              (List.exists reserved_public_keys ~f:(fun pk ->
                   Public_key.equal pk
                     (Public_key.decompress_exn @@ Account.public_key account)
               )) )
        |> List.map ~f:(fun (sk, account) ->
               ( Genesis_ledger.keypair_of_account_record_exn (sk, account)
               , account ) )
      in
      if with_snark then
        let accounts = List.take other_accounts 2 in
        let%bind block_count' =
          test_multiple_payments accounts (pks accounts) 15.
        in
        (*wait for a block after the ledger_proof is emitted*)
        let%map () =
          wait_until_cond
            ~f:(fun t -> block_count t > block_count')
            ~timeout:5.
        in
        assert (block_count coda > block_count')
      else
        let%bind _ =
          test_multiple_payments other_accounts (pks other_accounts) 7.
        in
        test_duplicate_payments sender_keypair receiver_keypair )

let command =
  let open Core in
  let open Async in
  Command.async ~summary:"Full coda end-to-end test"
    (Command.Param.return run_test)
