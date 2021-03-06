open Coda_base
open Core
open Async
open Signature_lib

(* TODO: Remove Transaction functor when we need to query transactions other
   than user_commands *)
module Make (Transaction : sig
  type t [@@deriving bin_io, compare, sexp, hash, to_yojson]

  include Comparable.S with type t := t

  include Hashable.S with type t := t

  val get_participants : t -> Public_key.Compressed.t list

  (* TODO: Remove Transaction functor when we query on actual transactions *)
  val on_delegation_command : t -> f:(Public_key.Compressed.t -> unit) -> unit
end) (Time : sig
  type t [@@deriving bin_io, compare, sexp]
end) =
struct
  module Database = Rocksdb.Serializable.Make (Transaction) (Time)

  module Txn_with_date = struct
    module T = struct
      type t = Transaction.t * Time.t [@@deriving sexp]

      let compare =
        Comparable.lift
          ~f:(fun (txn, date) -> (date, txn))
          [%compare: Time.t * Transaction.t]
    end

    include T
    include Comparable.Make (T)
  end

  type cache =
    { user_transactions: Txn_with_date.Set.t Public_key.Compressed.Table.t
    ; delegators: Txn_with_date.t Public_key.Compressed.Table.t
    ; all_transactions: Time.t Transaction.Table.t }

  type t = {database: Database.t; cache: cache; logger: Logger.t}

  let create logger directory =
    { database= Database.create ~directory
    ; cache=
        { user_transactions= Public_key.Compressed.Table.create ()
        ; all_transactions= Transaction.Table.create ()
        ; delegators= Public_key.Compressed.Table.create () }
    ; logger }

  (* TODO: make load function #2333 *)

  let close {database; _} = Database.close database

  let add
      { database
      ; cache= {all_transactions; user_transactions; delegators}
      ; logger } transaction date =
    match Hashtbl.find all_transactions transaction with
    | Some _retrieved_transaction ->
        Logger.trace logger
          !"Not adding transaction into transaction database since it already \
            exists: $transaction"
          ~module_:__MODULE__ ~location:__LOC__
          ~metadata:[("transaction", Transaction.to_yojson transaction)]
    | None ->
        Database.set database ~key:transaction ~data:date ;
        Hashtbl.add_exn all_transactions ~key:transaction ~data:date ;
        Transaction.on_delegation_command transaction ~f:(fun sender ->
            Hashtbl.set delegators ~key:sender ~data:(transaction, date) ) ;
        List.iter (Transaction.get_participants transaction) ~f:(fun pk ->
            let user_txns =
              Option.value
                (Hashtbl.find user_transactions pk)
                ~default:Txn_with_date.Set.empty
            in
            let user_txns' =
              Txn_with_date.Set.add user_txns (transaction, date)
            in
            Hashtbl.set user_transactions ~key:pk ~data:user_txns' )

  let get_delegator {cache= {delegators; _}; _} public_key =
    let open Option.Let_syntax in
    let%map delegation_user_command, _ = Hashtbl.find delegators public_key in
    delegation_user_command

  let get_total_transactions {cache= {user_transactions; _}; _} public_key =
    let open Option.Let_syntax in
    let%map transactions_with_dates_set =
      Hashtbl.find user_transactions public_key
    in
    Set.length transactions_with_dates_set

  let get_transactions {cache= {user_transactions; _}; _} public_key =
    let queried_transactions =
      let open Option.Let_syntax in
      let%map transactions_with_dates_set =
        Hashtbl.find user_transactions public_key
      in
      List.map (Txn_with_date.Set.to_list transactions_with_dates_set)
        ~f:(fun (txn, _) -> txn)
    in
    Option.value queried_transactions ~default:[]

  module With_non_null_transaction = struct
    let get_pagination_query ~f t public_key transaction =
      let open Option.Let_syntax in
      let queried_transactions_opt =
        let%bind transactions_with_dates =
          Hashtbl.find t.cache.user_transactions public_key
        in
        let%map transaction_with_date =
          let%map date = Hashtbl.find t.cache.all_transactions transaction in
          (transaction, date)
        in
        let earlier, transaction_opt, later =
          Set.split transactions_with_dates transaction_with_date
        in
        [%test_pred: Txn_with_date.t option]
          ~message:
            "Transaction should be in-memory cache database for public key"
          Option.is_some transaction_opt ;
        f earlier later
      in
      let%map transactions_with_dates, has_previous, has_next =
        queried_transactions_opt
      in
      (List.map transactions_with_dates ~f:fst, has_previous, has_next)

    let has_neighboring_page = Fn.compose not Set.is_empty

    let get_earlier_transactions t public_key transaction amount_to_query_opt =
      get_pagination_query t public_key transaction ~f:(fun earlier later ->
          let has_later = `Has_later_page (has_neighboring_page later) in
          let get_all_earlier_transactions_result () =
            (Set.to_list earlier, `Has_earlier_page false, has_later)
          in
          match amount_to_query_opt with
          | None ->
              get_all_earlier_transactions_result ()
          | Some n -> (
            match Set.nth earlier (Set.length earlier - n) with
            | None ->
                get_all_earlier_transactions_result ()
            | Some earliest_transaction ->
                let more_early_transactions, _, next_page_transactions =
                  Set.split earlier earliest_transaction
                in
                ( Set.to_list
                  @@ Set.add next_page_transactions earliest_transaction
                , `Has_earlier_page
                    (has_neighboring_page more_early_transactions)
                , has_later ) ) )

    let get_later_transactions t public_key transaction amount_to_query_opt =
      get_pagination_query t public_key transaction ~f:(fun earlier later ->
          let has_earlier = `Has_earlier_page (has_neighboring_page earlier) in
          let get_all_later_transactions_result () =
            (Set.to_list later, has_earlier, `Has_later_page false)
          in
          match amount_to_query_opt with
          | None ->
              get_all_later_transactions_result ()
          | Some n -> (
            match Set.nth later n with
            | None ->
                get_all_later_transactions_result ()
            | Some latest_transaction ->
                let next_page_transactions, _, _ =
                  Set.split later latest_transaction
                in
                ( Set.to_list next_page_transactions
                , has_earlier
                , `Has_later_page true ) ) )
  end

  let get_pagination_query ~get_default ~get_queries
      ({cache= {user_transactions; _}; _} as t) public_key transaction_opt
      amount_to_query_opt =
    let query_opt =
      match transaction_opt with
      | None -> (
          let open Option.Let_syntax in
          let%bind user_transactions =
            Hashtbl.find user_transactions public_key
          in
          let%bind default_transaction, _ = get_default user_transactions in
          match amount_to_query_opt with
          | None ->
              let%map queries, has_earlier_page, has_later_page =
                get_queries t public_key default_transaction None
              in
              (default_transaction :: queries, has_earlier_page, has_later_page)
          | Some amount_to_query when amount_to_query = 1 ->
              Some
                ( [default_transaction]
                , `Has_earlier_page false
                , `Has_later_page false )
          | Some amount_to_query ->
              let%map queries, has_earlier_page, has_later_page =
                get_queries t public_key default_transaction
                  (Some (amount_to_query - 1))
              in
              (default_transaction :: queries, has_earlier_page, has_later_page)
          )
      | Some transaction ->
          get_queries t public_key transaction amount_to_query_opt
    in
    Option.value query_opt
      ~default:([], `Has_earlier_page false, `Has_later_page false)

  let get_earlier_transactions =
    get_pagination_query ~get_default:Set.max_elt
      ~get_queries:With_non_null_transaction.get_earlier_transactions

  let get_later_transactions =
    get_pagination_query ~get_default:Set.min_elt
      ~get_queries:With_non_null_transaction.get_later_transactions
end

module User_command = struct
  include Coda_base.User_command.Stable.V1

  let get_participants user_command =
    let sender = User_command.sender user_command in
    let payload = User_command.payload user_command in
    let receiver =
      match User_command_payload.body payload with
      | Stake_delegation (Set_delegate {new_delegate}) ->
          new_delegate
      | Payment {receiver; _} ->
          receiver
    in
    [receiver; sender]

  module Gen = User_command.Gen

  let on_delegation_command user_command ~f =
    let sender = User_command.sender user_command in
    match User_command.(Payload.body @@ payload user_command) with
    | Payment _ ->
        ()
    | Stake_delegation _ ->
        f sender
end

let%test_module "Transaction_database" =
  ( module struct
    module Database = Make (User_command) (Int)

    let assert_transactions expected_transactions transactions =
      User_command.Set.(
        [%test_eq: t] (of_list expected_transactions) (of_list transactions))

    let ({Keypair.public_key= pk1; _} as keypair1) = Keypair.create ()

    let pk1 = Public_key.compress pk1

    let keypair2 = Keypair.create ()

    let logger = Logger.create ()

    let with_database ~trials ~f gen =
      Async.Thread_safe.block_on_async_exn
      @@ fun () ->
      Quickcheck.async_test gen ~trials ~f:(fun input ->
          File_system.with_temp_dir "/tmp/coda-test" ~f:(fun directory_name ->
              let database = Database.create logger directory_name in
              f input database ) )

    let add_all_transactions database all_transactions_with_dates =
      List.iter all_transactions_with_dates ~f:(fun (txn, time) ->
          Database.add database txn time )

    let extract_transactions transactions_with_dates =
      List.map transactions_with_dates ~f:(fun (txn, _) -> txn)

    let%test_unit "We can get all the transactions associated with a public key"
        =
      let trials = 10 in
      let time = 1 in
      with_database ~trials
        Quickcheck.Generator.(
          list
          @@ User_command.Gen.payment_with_random_participants
               ~keys:(Array.of_list [keypair1; keypair2])
               ~max_amount:10000 ~max_fee:1000 ())
        ~f:(fun user_commands database ->
          add_all_transactions database
            (List.map user_commands ~f:(fun txn -> (txn, time))) ;
          let pk1_expected_transactions =
            List.filter user_commands ~f:(fun user_command ->
                let participants =
                  User_command.get_participants user_command
                in
                List.mem participants pk1 ~equal:Public_key.Compressed.equal )
          in
          let pk1_queried_transactions =
            Database.get_transactions database pk1
          in
          assert_transactions pk1_expected_transactions
            pk1_queried_transactions ;
          Deferred.unit )

    module Pagination_test = struct
      module Gen = struct
        let gen_key_as_sender_or_receiver keypair1 keypair2 =
          let open Quickcheck.Generator.Let_syntax in
          if%map Bool.quickcheck_generator then (keypair1, keypair2)
          else (keypair2, keypair1)

        let key = gen_key_as_sender_or_receiver keypair1 keypair2

        let payment =
          User_command.Gen.payment ~key_gen:key ~max_amount:10000 ~max_fee:1000
            ()

        let payment_with_time lower_bound_incl upper_bound_incl =
          Quickcheck.Generator.tuple2 payment
            (Int.gen_incl lower_bound_incl upper_bound_incl)

        let max_number_txns_in_page = Int.gen_incl 1 10

        let non_empty_list gen =
          let open Quickcheck.Generator in
          let open Let_syntax in
          let%map x = gen and xs = list gen in
          x :: xs

        let transaction_test_input =
          let time = 50 in
          let earliest_time_in_next_page = time - 10 in
          let max_time = 2 * time in
          let open Quickcheck.Generator in
          let open Let_syntax in
          let%bind transactions_per_page = max_number_txns_in_page in
          tuple4
            ( non_empty_list
            @@ payment_with_time 0 (earliest_time_in_next_page - 1) )
            ( list_with_length transactions_per_page
            @@ payment_with_time earliest_time_in_next_page (time - 1) )
            (payment_with_time time time)
            (non_empty_list @@ payment_with_time (time + 1) max_time)

        let transaction_test_cutoff_input =
          let time = 50 in
          let open Quickcheck.Generator in
          let open Let_syntax in
          let%bind transactions_per_page = max_number_txns_in_page in
          tuple4
            ( list_with_length transactions_per_page
            @@ payment_with_time 0 (time - 1) )
            (payment_with_time time time)
            (non_empty_list @@ payment_with_time (time + 1) Int.max_value)
            (Int.gen_incl 0 10)

        let test_no_transaction_input =
          let open Quickcheck.Generator in
          let open Quickcheck.Generator.Let_syntax in
          let%bind num_transactions = Int.gen_incl 2 20 in
          let%bind transactions =
            list_with_length num_transactions
              (tuple2 payment Int.quickcheck_generator)
          in
          let%bind amount_to_query = Int.gen_incl 1 (num_transactions - 1) in
          tuple2
            (Quickcheck.Generator.of_list [None; Some amount_to_query])
            (return transactions)
      end

      type query_next_page =
           Database.t
        -> Public_key.Compressed.t
        -> User_command.t option
        -> int option
        -> User_command.t list
           * [`Has_earlier_page of bool]
           * [`Has_later_page of bool]

      let test ~trials gen ~(query_next_page : query_next_page) =
        with_database gen ~trials
          ~f:(fun ( earlier_transactions_with_dates
                  , next_page_transactions_with_dates
                  , ((query_transaction, _) as query_transaction_with_time)
                  , later_transactions_with_dates )
             database
             ->
            let all_transactions_with_dates =
              earlier_transactions_with_dates
              @ next_page_transactions_with_dates
              @ (query_transaction_with_time :: later_transactions_with_dates)
            in
            add_all_transactions database all_transactions_with_dates ;
            let expected_next_page_transactions =
              extract_transactions next_page_transactions_with_dates
            in
            let ( next_page_transactions
                , `Has_earlier_page has_earlier
                , `Has_later_page has_later ) =
              query_next_page database pk1 (Some query_transaction)
                (Some (List.length next_page_transactions_with_dates))
            in
            assert_transactions expected_next_page_transactions
              next_page_transactions ;
            assert has_earlier ;
            assert has_later ;
            Deferred.unit )

      let test_no_amount_input ~trials gen ~(query_next_page : query_next_page)
          ~check_pages =
        with_database gen ~trials
          ~f:(fun ( earlier_transactions_with_dates
                  , next_page_transactions_with_dates
                  , ((query_transaction, _) as query_transaction_with_time)
                  , later_transactions_with_dates )
             database
             ->
            let all_transactions_with_dates =
              earlier_transactions_with_dates
              @ next_page_transactions_with_dates
              @ (query_transaction_with_time :: later_transactions_with_dates)
            in
            add_all_transactions database all_transactions_with_dates ;
            let expected_next_page_transactions =
              extract_transactions
                ( earlier_transactions_with_dates
                @ next_page_transactions_with_dates )
            in
            let ( next_page_transactions
                , `Has_earlier_page has_earlier
                , `Has_later_page has_later ) =
              query_next_page database pk1 (Some query_transaction) None
            in
            assert_transactions expected_next_page_transactions
              next_page_transactions ;
            check_pages has_earlier has_later ;
            Deferred.unit )

      let test_no_transaction_input ~trials gen
          ~(query_next_page : query_next_page) ~check_pages ~compare =
        with_database gen ~trials
          ~f:(fun (amount_to_query_opt, transactions_with_dates) database ->
            Option.iter amount_to_query_opt ~f:(fun amount_to_query ->
                assert (List.length transactions_with_dates >= amount_to_query)
            ) ;
            add_all_transactions database transactions_with_dates ;
            let expected_next_page_transactions =
              let sorted_transaction_with_dates =
                List.sort ~compare transactions_with_dates
              in
              let sorted_transactions =
                extract_transactions sorted_transaction_with_dates
              in
              Option.value_map amount_to_query_opt ~default:sorted_transactions
                ~f:(List.take sorted_transactions)
            in
            let ( next_page_transactions
                , `Has_earlier_page has_earlier
                , `Has_later_page has_later ) =
              query_next_page database pk1 None amount_to_query_opt
            in
            assert_transactions expected_next_page_transactions
              next_page_transactions ;
            check_pages has_earlier has_later ;
            Deferred.unit )

      let test_with_cutoff ~trials ~(query_next_page : query_next_page) gen
          ~check_pages =
        with_database ~trials gen
          ~f:(fun ( earlier_transactions_with_dates
                  , ( (querying_transaction, _) as
                    querying_transaction_with_date )
                  , later_transactions_with_dates
                  , offset )
             database
             ->
            let all_transactions_with_dates =
              earlier_transactions_with_dates
              @ querying_transaction_with_date :: later_transactions_with_dates
            in
            add_all_transactions database all_transactions_with_dates ;
            let expected_next_page_transactions =
              extract_transactions earlier_transactions_with_dates
            in
            let amount_to_query =
              List.length expected_next_page_transactions + offset
            in
            let ( next_page_transactions
                , `Has_earlier_page has_earlier
                , `Has_later_page has_later ) =
              query_next_page database pk1 (Some querying_transaction)
                (Some amount_to_query)
            in
            assert_transactions expected_next_page_transactions
              next_page_transactions ;
            check_pages has_earlier has_later ;
            Deferred.unit )

      let%test_unit "Get n transactions that were added before an arbitrary \
                     transaction" =
        test ~trials:10 Gen.transaction_test_input
          ~query_next_page:Database.get_earlier_transactions

      let%test_unit "Trying to query n transactions that occurred before \
                     another transaction can give you less than n transactions"
          =
        test_with_cutoff ~trials:5 Gen.transaction_test_cutoff_input
          ~query_next_page:Database.get_earlier_transactions
          ~check_pages:(fun has_earlier has_later ->
            [%test_result: bool]
              ~message:
                "We should not have anymore earlier transactions to query"
              ~equal:Bool.equal ~expect:false has_earlier ;
            [%test_result: bool]
              ~message:"We should have at least one later transaction"
              ~equal:Bool.equal ~expect:true has_later )

      let%test_unit "Get the n most latest transactions if transactions are \
                     not provided" =
        test_no_transaction_input ~trials:5 Gen.test_no_transaction_input
          ~query_next_page:Database.get_earlier_transactions
          ~check_pages:(fun _has_earlier has_later -> assert (not has_later))
          ~compare:
            (Comparable.lift
               ~f:(fun (txn, date) -> (-1 * date, txn))
               [%compare: int * User_command.t])

      let%test_unit "Get all transactions that were added before an arbitrary \
                     transaction" =
        test_no_amount_input ~trials:2 Gen.transaction_test_input
          ~query_next_page:Database.get_earlier_transactions
          ~check_pages:(fun has_earlier has_later ->
            assert (not has_earlier) ;
            assert has_later )

      let invert_transaction_time =
        List.map ~f:(fun (txn, date) -> (txn, -1 * date))

      let later_pagination_transaction_gen =
        let open Quickcheck.Generator.Let_syntax in
        let%map ( earlier_transactions_with_dates
                , next_page_transactions_with_dates
                , (query_transaction, time)
                , later_transactions_with_dates ) =
          Gen.transaction_test_input
        in
        ( invert_transaction_time earlier_transactions_with_dates
        , invert_transaction_time next_page_transactions_with_dates
        , (query_transaction, -1 * time)
        , invert_transaction_time later_transactions_with_dates )

      let%test_unit "Get n transactions that were added after an arbitrary \
                     transaction" =
        test ~trials:5 later_pagination_transaction_gen
          ~query_next_page:Database.get_later_transactions

      let%test_unit "Trying to query n transactions that occurred after \
                     another transaction can give you less than n transactions"
          =
        let later_pagination_transaction_gen =
          let open Quickcheck.Generator.Let_syntax in
          let%map ( earlier_transactions_with_dates
                  , (querying_transaction, time)
                  , later_transactions_with_dates
                  , offsets ) =
            Gen.transaction_test_cutoff_input
          in
          ( invert_transaction_time earlier_transactions_with_dates
          , (querying_transaction, -1 * time)
          , invert_transaction_time later_transactions_with_dates
          , offsets )
        in
        test_with_cutoff ~trials:5 later_pagination_transaction_gen
          ~query_next_page:Database.get_later_transactions
          ~check_pages:(fun has_earlier has_later ->
            [%test_result: bool]
              ~message:
                "We should have at least one earlier transactions to query"
              ~equal:Bool.equal ~expect:true has_earlier ;
            [%test_result: bool]
              ~message:"We should not be able to query any more later queries"
              ~equal:Bool.equal ~expect:false has_later )

      let%test_unit "Get all transactions that were added after an arbitrary \
                     transaction" =
        test_no_amount_input ~trials:2 later_pagination_transaction_gen
          ~query_next_page:Database.get_later_transactions
          ~check_pages:(fun has_earlier has_later ->
            assert has_earlier ;
            assert (not has_later) )

      let compare_txns_with_dates =
        Comparable.lift
          ~f:(fun (txn, date) -> (date, txn))
          [%compare: int * User_command.t]

      let%test_unit "Get the n most earliest transactions if transactions are \
                     not provided" =
        test_no_transaction_input ~trials:5 Gen.test_no_transaction_input
          ~query_next_page:Database.get_later_transactions
          ~check_pages:(fun has_earlier _has_later -> assert (not has_earlier))
          ~compare:compare_txns_with_dates

      let%test_unit "Get the most recent delegator" =
        let keys =
          Array.init 3 ~f:(fun _ -> Signature_lib.Keypair.create ())
        in
        let key_gen =
          let open Quickcheck.Generator.Let_syntax in
          let%map reciever = Quickcheck_lib.of_array keys in
          (keypair1, reciever)
        in
        let gen_stake_delegation =
          User_command.Gen.stake_delegation ~key_gen ~max_fee:1 ()
        in
        let gen_user_commands_with_time =
          let open Quickcheck.Generator.Let_syntax in
          let%bind num_commands = Int.gen_incl 1 50 in
          let%bind time = Int.gen_incl 0 10 in
          let%bind stake_delegation = gen_stake_delegation in
          Quickcheck.Generator.list_with_length num_commands
          @@ Quickcheck.Generator.return (stake_delegation, time)
        in
        with_database ~trials:10 gen_user_commands_with_time
          ~f:(fun commands_with_time database ->
            add_all_transactions database commands_with_time ;
            let most_recent_delegation, _ =
              Option.value_exn
                (List.max_elt commands_with_time
                   ~compare:compare_txns_with_dates)
            in
            Deferred.return
            @@ [%test_eq: User_command.t] ~equal:User_command.equal
                 most_recent_delegation
                 (Option.value_exn (Database.get_delegator database pk1)) )
    end
  end )

module T = Make (User_command) (Block_time.Time.Stable.V1)
include T
