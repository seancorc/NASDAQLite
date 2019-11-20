open OrderBook
open AccountManager

module type MatchingEngine = sig 
  type t
  val create : unit -> t
  val member : t -> string -> bool
  val execute_regular_order : t -> order_direction -> order -> string -> unit
  val execute_market_order : t -> order_direction -> string -> int -> string -> unit
  val tickers : t -> string list
  val get_order_book : t -> string -> OrderBook.t
  val load_from_dir : string -> t
  val get_account_manager : t -> AccountManager.t
end

exception UnboundTicker

(** [StringHash] is a module representing a HashedType using String as the key*)
module StringHash = struct
  type t = string

  (** [equal i j] is true if i is structurally equal to j and false otherwise *)
  let equal i j = i = j

  (** [hash i] is i after running a hashing algorithm on it for use in 
      Hashtbl *)
  let hash i = Hashtbl.hash i
end


module D = Hashtbl.Make(StringHash)

module MatchingEngine : MatchingEngine = struct

  type t = {
    orderbooks: OrderBook.t D.t;
    account_manager: AccountManager.t;
  }

  let aapl = OrderBook.empty

  let no_tickers = D.create 20

  let create _ = 
    let obs = D.create 20 in 
    D.replace obs "AAPL" OrderBook.empty;
    D.replace obs "GOOG" OrderBook.empty;
    D.replace obs "MSFT" OrderBook.empty;
    D.replace obs "AMZN" OrderBook.empty;
    D.replace obs "ROKU" OrderBook.empty;
    let am = AccountManager.create () in 
    {
      orderbooks = obs;
      account_manager = am;
    }

  let get_account_manager (me: t) = me.account_manager

  let member (me: t) (ticker: string) : bool = 
    let obs = me.orderbooks in D.mem obs ticker

  let tickers (me: t) : string list = 
    let obs = me.orderbooks in 
    D.fold (fun ticker ob acc -> ticker :: acc) obs []

  let get_order_book (me: t) (ticker: string) : OrderBook.t = 
    if member me ticker then 
      let obs = me.orderbooks in 
      let ob = D.find obs ticker in 
      ob
    else raise UnboundTicker

  let receive_order (me: t) (direction: order_direction) (order: order) 
      (ticker: string) : unit = 
    if member me ticker then 
      let obs = me.orderbooks in 
      let ob = D.find obs ticker in 
      let submit_order = (direction, order) in 
      let ob' = OrderBook.insert_order ob submit_order in 
      D.replace obs ticker ob'
    else raise UnboundTicker

  let populate_orders_for_direction me ticker direction ic =
    let line = input_line ic in
    if line = "endticker" then () else 
      let lst = String.split_on_char ',' line in
      let rec add_orders lst =
        match lst with 
        | [] -> ()
        | u :: a :: p :: t :: r -> 
          let amount = int_of_string a in
          let price = float_of_string p in
          let time = float_of_string t in
          let order = (u,amount,price,time) in
          receive_order me direction order ticker  
        | _ -> raise (Invalid_argument "Bad File") 
      in
      add_orders lst

  let rec populate_ticker_for_direction me ic ticker direction = 
    populate_orders_for_direction me ticker direction ic; 
    ()

  let rec populate_engine me ic = 
    try 
      let ticker = input_line ic in
      let direction = if (input_line ic) = "buy" then Buy else Sell in
      populate_ticker_for_direction me ic ticker direction;
      let next_direction = if (input_line ic) = "buy" then Buy else Sell in
      populate_ticker_for_direction me ic ticker next_direction;
      populate_engine me ic;
    with e ->
      if e = End_of_file then ()
      else raise e

  (** Searches for file 'orders.csv' and returns input_channel associated 
      with that file *)
  let rec get_input_channel handle dirname = 
    let filename = Unix.readdir handle in
    match filename with 
    | "orders.csv" -> 
      open_in (dirname ^ Filename.dir_sep ^ filename)
    | s -> get_input_channel handle dirname

  let load_from_dir dirname = 
    let ic = get_input_channel (Unix.opendir dirname) dirname in
    let me = create () in
    populate_engine me ic;
    me


  let rec repeat_parse (ob: OrderBook.t) (acc: transaction list) 
    : (transaction list) * OrderBook.t = 
    let tx_opt, ob' = OrderBook.construct_tx ob in 
    match tx_opt with 
    | Some tx -> 
      begin
        let acc' = tx :: acc in 
        repeat_parse ob' acc'
      end
    | None -> acc, ob'

  let parse_order_book (me: t) (ticker: string) 
    : (transaction list) * OrderBook.t = 
    let ob = get_order_book me ticker in 
    repeat_parse ob []

  let update_orderbook (me: t) (ticker: string) (ob: OrderBook.t) : unit = 
    D.replace me.orderbooks ticker ob 

  let process_transaction (am: AccountManager.t) (tx: float * int * string * string) (ticker: string) : unit = 
    match tx with 
    | p, amt, b, s -> 
      begin 
        let amt' = float_of_int amt in 
        let total_amount = p *. amt' in 
        let _ = AccountManager.dec_account_balance am b total_amount in 
        let _ = AccountManager.inc_account_balance am s total_amount in 
        let _ = AccountManager.inc_account_position am b ticker amt in 
        let _ = AccountManager.dec_account_position am s ticker amt in 
        ()
      end


  let rec process_transactions (am: AccountManager.t) (txs: transaction list) (ticker: string) : unit = 
    match txs with 
    | tx :: t -> 
      process_transaction am tx ticker;
      process_transactions am t ticker;
      ()
    | [] -> ()

  let execute_regular_order (me: t) (direction: order_direction) (order: order) 
      (ticker: string) : unit = 
    let am = me.account_manager in 
    let _ = receive_order me direction order ticker in 
    let (txs, ob') = parse_order_book me ticker in 
    let _ = process_transactions am txs ticker in 
    let _ = update_orderbook me ticker ob' in
    ()

  let execute_market_order (me: t) (direction: order_direction) (ticker: string) (amount: int) (addr: string): unit = 
    let order = match direction with 
      | Buy -> (addr, amount, max_float, Unix.time ())
      | Sell -> (addr, amount, min_float, Unix.time ())in 
    let _ = execute_regular_order me direction order ticker in 
    ()
end
