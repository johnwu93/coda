open Core
open Async
open Coda_worker
open Coda_inputs
open Coda_base

let name = "coda-delegation-test"

let wait_minutes n logger =
  let rec loop remaining =
    if remaining <= 0 then Deferred.unit
    else (
      Logger.info logger ~module_:__MODULE__ ~location:__LOC__
        "Still sleeping (yawn)" ;
      let%bind () = after (Time.Span.of_min 1.) in
      loop (remaining - 1) )
  in
  loop n

let main () =
  let logger = Logger.create () in
  let n = 3 in
  let snark_work_public_keys ndx =
    List.nth_exn Genesis_ledger.accounts ndx
    |> fun (_, acct) -> Some (Account.public_key acct)
  in
  let%bind testnet =
    Coda_worker_testnet.test logger n Option.some snark_work_public_keys
      Protocols.Coda_pow.Work_selection.Seq ~max_concurrent_connections:None
  in
  (* zeroth account will be delegatee *)
  let keypairs =
    Genesis_ledger.(
      List.map (List.tl_exn accounts) ~f:keypair_of_account_record_exn)
  in
  let _ =
    List.iteri Genesis_ledger.accounts (fun i (_, acct) ->
        Stdlib.Printf.eprintf
          !"ACCT: %d KEY: %{sexp: Account.key} BALANCE: %{sexp: \
            Currency.Balance.t}\n\
            %!"
          i (Account.public_key acct) acct.balance )
  in
  (* second account is wealthy sender *)
  let ((_, sender_account) as sender) =
    List.nth_exn Genesis_ledger.accounts 2
  in
  let sender_pubkey = Account.public_key sender_account in
  let sender_keypair = Genesis_ledger.keypair_of_account_record_exn sender in
  let worker = testnet.workers.(0) in
  let%bind sender_transition_reader =
    Coda_process.new_block_exn worker sender_pubkey
  in
  (* send payments *)
  Deferred.don't_wait_for
    (Pipe_lib.Linear_pipe.iter sender_transition_reader ~f:(fun transition ->
         let proposer =
           Coda_transition.External_transition.proposer transition
         in
         return
         @@ Stdlib.Printf.eprintf
              !"GOT A TRANSITION %{sexp: Coda_transition.External_transition.t}\n\
                PROPOSED BY: %{sexp:Account.key}\n\
                SENDER: %{sexp: Account.key}\n\
                SAME AS SENDER: %B\n\n\
                %!"
              transition proposer sender_pubkey
              (Signature_lib.Public_key.Compressed.equal proposer sender_pubkey)
     )) ;
  (*  let%bind () =
    Coda_worker_testnet.Payments.send_several_payments testnet ~node:0
      ~keypairs ~n:4
  in *)
  let ((_, delegatee_account) as delegatee) =
    List.nth_exn Genesis_ledger.accounts 0
  in
  let delegatee_pubkey = Account.public_key delegatee_account in
  let%bind delegatee_transition_reader =
    Coda_process.new_block_exn worker delegatee_pubkey
  in
  Deferred.don't_wait_for
    (Pipe_lib.Linear_pipe.iter delegatee_transition_reader
       ~f:(fun transition ->
         let proposer =
           Coda_transition.External_transition.proposer transition
         in
         return
         @@ Stdlib.Printf.eprintf
              !"GOT A TRANSITION %{sexp: Coda_transition.External_transition.t}\n\
                PROPOSED BY: %{sexp:Account.key}\n\
                DELEGATEE: %{sexp: Account.key}\n\
                SAME AS DELEGATEE: %B\n\n\
                %!"
              transition proposer delegatee_pubkey
              (Signature_lib.Public_key.Compressed.equal proposer
                 delegatee_pubkey) )) ;
  (*  let%bind () =
    Coda_worker_testnet.Payments.send_several_payments testnet ~node:0
      ~keypairs ~n:4
  in *)
  let%bind () =
    Coda_worker_testnet.Delegates.delegate_stake testnet ~node:0
      ~delegator:sender_keypair.private_key ~delegatee:delegatee_pubkey
  in
  let%bind () = wait_minutes 30 logger in
  let%bind () =
    Coda_worker_testnet.Payments.send_several_payments testnet ~node:0
      ~keypairs ~n:4
  in
  let%bind () = after (Time.Span.of_min 10.) in
  Coda_worker_testnet.Api.teardown testnet

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:
      "Test whether delegation from a low-balance node to a high-balance node \
       works"
    (Command.Param.return main)
